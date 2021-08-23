namespace LLIR

open LLVM
open LLIR
open LLIR.Core
open Common

exception CallGraphException of string

type Calls = Map<VarID, ID [] option>

type CGState = {
  Prog: Program
  Func: Function
  Args: MD []
  VarState: Map<VarID, MD>
  Calls: Calls
}

type CallGraph = Map<ID, Calls>

module Calls =
  let empty = Map.empty

  let getTargets calls =
    Map.toArray calls |> Array.map (snd >> Array.ofOpt) |> Array.concat

  let debug calls =
    let iter fr targets =
      printf "  %A -> [" fr
      match targets with
      | Some targets -> Array.iter (printf "%s, ") targets
      | _ -> ()
      printfn "]"
    Map.iter iter calls

module CGState =
  let reduceMD (state: CGState) md = Metadata.reduceMD state.Prog md

  let getRawMD state ptr = Program.getMD state.Prog ptr
  let getMD state ptr = reduceMD state (ptr, getRawMD state ptr)

  let reduceState state =
    let args = state.Args
    let vstate = state.VarState
    { state with Args = Array.map (reduceMD state) args
                 VarState = Map.map (fun _ md -> reduceMD state md) vstate }

  let addVar id state md =
    let md = reduceMD state md
    { state with VarState = Map.add id md state.VarState }, md

  let tryAdd state id = function
    | Some md -> Map.add id md state
    | _ -> state

  let applyCall state vID = function
    | Some [|fID|] ->
      match Program.getFunc state.Prog fID |> Function.getRetMD with
      | Some ret -> addVar vID state ret |> fst
      | _ -> state
    | _ -> state

  let addCall vID targets state =
    let state = applyCall state vID targets
    { state with Calls = Map.add vID targets state.Calls }

  let getCalls state = state.Calls

  let getArgs state = state.Args

  let addExpr state md = function
    | Var (id, _) -> Map.add id md state
    | Addr (Var (id, _), Num bv, _) when BitVector.isZero bv ->
      Map.add id md state
    | _ -> state

  let mkPtr (ptr, _) = -1, Metadata.DerivedType (Ptr, "", ptr, 0)

  let handleDbg prog state = function
    | [| Expr.Metadata (_, LLVM.LocalAs (_, _, id))
         Expr.Metadata md
         Expr.Metadata _ |] -> Map.add id (mkPtr md) state
    | _ -> state

  let handleCall prog state retID fID args =
    let func = Program.getFunc prog fID
    let state = Function.getRetMD func |> tryAdd state retID
    let mds = Function.getArgsMD func
    if Array.length mds = Array.length args then
      Array.fold2 addExpr state mds args
    else state

  let buildVarState prog state = function
    | Call (_, Callee.ID "llvm.dbg.declare", args, _, _)
    | Call (_, Callee.ID "llvm.dbg.value", args, _, _) ->
      handleDbg prog state args
    | Call (ret, Callee.ID f, args, _, _) when Program.isDeclr prog f |> not ->
      handleCall prog state ret f args
    | Call (ret, Callee.Expr (GlobalVar (f, _)), args, _, _)
      when Program.hasFunc prog f && Program.isDeclr prog f |> not ->
      handleCall prog state ret f args
    | _ -> state

  let empty prog func args state = {
    Prog = prog
    Func = func
    Args = args
    VarState = state
    Calls = Calls.empty
  }

  let buildArgs func args state =
    let argc = Function.getArgc func
    let argc_ = Array.length args
    if argc > argc_ then
      let args = Array.append args (Array.create (argc - argc_) AST.todoMD)
      let iter (bID, idx) md =
        if bID = -1 && idx >= 0 then Array.set args idx md else ()
      Map.iter iter state
      args
    else args

  let init prog func =
    let args = Function.getArgsMD func
    let state = Function.foldStmt (buildVarState prog) Map.empty func
    let args = buildArgs func args state
    reduceState (empty prog func args state)

  let debug cg =
    let iterArg idx (ptr, md) =
      printf "Args[%d] = %s" idx (Pp.printMetadata ptr md)
    let iterVar id (ptr, md) =
      printf "%s = %s" (Pp.ppVarID id) (Pp.printMetadata ptr md)
    printfn "----------------------------"
    printfn "%s" (Function.getID cg.Func)
    Array.iteri iterArg cg.Args
    Map.iter iterVar cg.VarState

  let getStmt state (bID, iID) =
    let block = Array.get (Function.getBody state.Func) bID
    Array.get block iID

  let evalMDLoad state (ptr, md) =
    match md with
    | DerivedType (kind, _, ptr, _) when Metadata.isReducedKind kind |> not ->
      state, getMD state ptr
    | SubProgram _ -> state, (ptr, md)
    | _ -> state, AST.todoMD
    (*| Metadata.TODO -> state, AST.todoMD
    | x ->
      printfn "%A" x
      debug state
      failwith "todo"
    *)

  let vtableFinder state offset elem =
    let md = getRawMD state elem
    match md with
    | SubProgram (_, _, _, Some idx) when offset = 64 * idx -> Some (elem, md)
    | _ -> None

  let elemFinder state offset elem =
    let md = getRawMD state elem
    match md with
    | DerivedType (_, _, _, idx)
    | CompositeType (_, _, _, _, _, idx) when offset = idx -> Some (elem, md)
    | _ -> None

  let findAddr finder state offset elems =
    match Array.tryPick (finder state offset) elems with
    | Some md -> reduceMD state md
    | _ -> AST.todoMD

  let evalMDAddr offset state (_, md) =
    match md with
    | CompositeType (_, _, _, _, elems, _) ->
      state, findAddr vtableFinder state offset elems
    | DerivedType (kind, _, ty, _) when Metadata.isReducedKind kind |> not ->
      match getMD state ty |> snd with
      | CompositeType (_, _, _, _, elems, _) ->
        state, findAddr elemFinder state offset elems |> mkPtr
      | _ -> state, AST.todoMD
    | _ -> state, AST.todoMD

  let rec calcExpr state = function
    | Var (id, _) -> calcVar state id
    | Arg (idx, _) -> state, Array.get state.Args idx
    | Load (expr, _, _) -> calcExpr state expr ||> evalMDLoad
    | Addr (expr, Num bv, _) -> BitVector.toInt bv |> calcAddr state expr
    | GlobalVar (id, _) ->
      if Program.hasFunc state.Prog id then
        state, Program.getFuncMD state.Prog id
      else state, Program.getGlobalMD state.Prog id |> mkPtr
    | _ -> state, AST.todoMD

  and calcVar state id =
    match Map.tryFind id state.VarState with
    | Some md -> state, md
    | _ -> getStmt state id |> calcStmt state id

  and calcAddr state addr offset = calcExpr state addr ||> evalMDAddr offset

  and calcStmt state id = function
    | Def (id, expr) -> calcExpr state expr ||> addVar id
    | Phi (_, map, _, _) -> calcPhi state map ||> addVar id
    | _ -> state, AST.todoMD

  and calcPhiFolder (state, ret) _ expr =
    if ret = [] then
      let state, md = calcExpr state expr
      match (snd md) with
      | TODO _ -> state, ret
      | _ -> state, [md]
    else state, ret

  and calcPhi state map =
    let state, ret = Map.fold calcPhiFolder (state, []) map
    match ret with
    | [md] -> state, md
    | _ -> state, AST.todoMD

  let isSubMD state large small = Metadata.isSubMD state.Prog large small

  let findFuncs state name p1 =
    let prog = state.Prog
    let finder acc p2 = function
      | SubProgram (n, fID, ty_, i) as m ->
        if n = name && Program.hasFunc prog fID && isSubMD state p1 p2 then
          fID :: acc
        else acc
      | _ -> acc
    match Program.getMDs state.Prog |> Map.fold finder [] with
    | [] -> None
    | ret -> Set.ofList ret |> Set.toArray |> Some

  let resolveTargets state (ptr, md) =
    match md with
    | SubProgram (_, fID, _, _) when Program.hasFunc state.Prog fID ->
      Some [|fID|]
    | SubProgram (name, _, _, _) -> findFuncs state name ptr
    | _ -> None

module CallGraph =
  let private analyzeOne state vID = function
    | Call (_, Callee.ID fID, _, _, _) ->
      CGState.addCall vID (Some [|fID|]) state
    | Call (_, Callee.Expr (GlobalVar (fID, _)), _, _, _)
      when Program.hasFunc state.Prog fID ->
      CGState.addCall vID (Some [|fID|]) state
    | Call (_, Callee.Expr expr, _, _, _) ->
      let state, md = CGState.calcExpr state expr
      CGState.addCall vID (CGState.resolveTargets state md) state
    | _ -> state

  let private analyzeFunc prog id func =
    let state = CGState.init prog func
    let state = Function.foldStmtIdx analyzeOne state func
    let args = CGState.getArgs state
    let md = fst func.Metadata, { snd func.Metadata with  Args = args }
    { func with Calls = CGState.getCalls state
                Metadata = md }

  let private asyncAnalyzeFunc prog (id, func) = async {
    return id, analyzeFunc prog id func
  }

  let analyze prog =
    Logger.info "Start CallGraph analysis"
    Program.getFuncs prog |> Map.toArray
    |> Utils.doParallel (asyncAnalyzeFunc prog)
    |> Map.ofArray
    |> Program.setFuncs prog

  let getRev prog =
    let map _ func =
      Function.getCalls func |> Map.toArray |> Array.map (snd >> Array.ofOpt)
      |> Array.concat |> Set.ofArray
    let fold ret caller callees =
      let add ret callee =
        let set = Map.findSet callee ret |> Set.add caller
        Map.add callee set ret
      Set.fold add ret callees
    Program.getFuncs prog |> Map.map map |> Map.fold fold Map.empty

  let debug prog =
    let iter id func =
      printfn "----------------------------"
      printfn "%s" id
      Function.getCalls func |> Calls.debug
    Program.getFuncs prog |> Map.iter iter
