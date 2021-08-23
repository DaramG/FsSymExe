namespace PathOptimizer.ConstantFolding

open LLIR
open LLIR.Core
open Common
open PathOptimizer

type VarConst = Map<int, RetConst option> * Map<VarID, RetConst option>

type State<'T when 'T: comparison> = {
  Prog: Program
  FuncID: ID
  Args: Expr []
  ConstMemory: Map<Expr, Expr>
  Memory: Map<Expr, Expr>
  Vars: Map<VarID, Expr>
  ICFG: ICFG<'T>
  Edges: Edges
  InProc: Set<ID * ConstArgs * Edges * 'T>
  DefaultKey: 'T
  RetVal: Expr option * bool (* IsConst *)
  RetConst: RetConst option
  VarConst: VarConst
  Cache: CacheICFG<'T>
  GetNxtCFG: State<'T> ->(int * BlockID) -> ID -> ConstArgs -> ICFG<'T> option
}

module VarConst =
  let empty = Map.empty, Map.empty

  let addVar varID c (m1, m2) =
    match Map.tryFind varID m2 with
    | Some (Some x) ->
      if x = c then m1, m2 else m1, Map.add varID None m2
    | Some None -> m1, m2
    | None -> m1, Map.add varID (Some c) m2

  let addArg idx c (m1, m2) =
    match Map.tryFind idx m1 with
    | Some (Some x) ->
      if x = c then m1, m2 else Map.add idx None m1, m2
    | Some None -> m1, m2
    | None -> Map.add idx (Some c) m1, m2

  let getVar varID (m1, m2) =
    match Map.tryFind varID m2 with
    | Some ret -> ret
    | None -> None

  let getArg idx (m1, m2) =
    match Map.tryFind idx m1 with
    | Some ret -> ret
    | None -> None

  let applyArgs args (m1, _) =
    let length = Array.length args
    let iter idx v =
      if idx < length then
        match v, Array.get args idx with
        | Some (expr, true), Arg _ -> Array.set args idx expr
        | _ -> ()
      else ()
    Map.iter iter m1
    args

  let mergeArgs args const_ =
    let folder ret idx (arg: RetConst option) =
      match ret with
      | Some (m1, m2) ->
        match Map.tryFind idx m1, arg with
        | Some (Some (cond, true)), Some (argExpr, flag) ->
          if (flag && cond <> argExpr) || ((not flag) && cond = argExpr) then
            None
          else Some (m1, m2)
        | Some (Some (cond, false)), Some (argExpr, flag) ->
          let argExpr, flag = Option.get arg
          if flag && cond = argExpr then None
          else Some (m1, m2)
        | _, Some _ -> Some (Map.add idx arg m1, m2)
        | _ -> Some (m1, m2)
      | _ -> None
    Map.fold folder (Some const_) args

  let debug fID (m1, m2) =
    printfn "%s:" fID
    let iter varID = function
      | Some (expr, flag) ->
        printfn "%A -> %s, %A" varID (Pp.printExpr expr) flag
      | _ -> ()
    Map.iter iter m1
    Map.iter iter m2

module State =

  let defaultGetNxtCFG state instPos fID cArgs =
    match ICFG.tryGetCall state.ICFG instPos with
    | Some cfg -> Some cfg
    | _ ->
      Program.getFunc state.Prog fID |> ICFG.ofFunc state.DefaultKey |> Some

  let getNxtCFG state instrPos fID cArgs =
    state.GetNxtCFG state instrPos fID cArgs

  let setGetNxtCFG state func = { state with GetNxtCFG = func }

  let init prog defaultKey cache = {
    Prog = prog
    FuncID = ""
    Args = [||]
    ConstMemory = Map.empty
    Memory = Map.empty
    Vars = Map.empty
    ICFG = ICFG.dummy "" defaultKey
    Edges = Map.empty
    InProc = Set.empty
    DefaultKey = defaultKey
    RetVal = None, true
    RetConst = None
    VarConst = VarConst.empty
    Cache = cache
    GetNxtCFG = defaultGetNxtCFG
  }

  let getArg state idx = Array.get state.Args idx

  let addVar state id expr = { state with Vars = Map.add id expr state.Vars }

  let getVar state id expr =
    match Map.tryFind id state.Vars with
    | Some expr -> expr
    | _ -> expr

  let addMem (state: State<'T>) addr value =
    { state with Memory = Map.add addr value state.Memory }

  let loadMem state addr size =
    match Map.tryFind addr state.ConstMemory with
    | Some value when size <= Builder.getSize value ->
      Builder.mkExtract value Builder.zero32 size
    | _ ->
      match Map.tryFind addr state.Memory with
      | Some value when size <= Builder.getSize value ->
        Builder.mkExtract value Builder.zero32 size
      | _ -> Builder.mkUndef size

  let flushMem (state: State<'T>) = { state with Memory = Map.empty }

  let doReturn state bID e =
    let state = { state with ICFG = ICFG.addRet bID state.ICFG }
    match e with
    | Some value when (snd state.RetVal) ->
      match fst state.RetVal with
      | Some v -> { state with RetVal = Some v, v = value }
      | _ -> { state with RetVal = Some value, true }
    | _ -> state

  let removeBlock state bID =
    { state with ICFG = ICFG.removeNode state.ICFG bID }

  let removeEdge state src dst =
    { state with ICFG = ICFG.removeEdge state.ICFG src dst }

  let getCFG state = state.ICFG

  let addCall state instrID retID cfg =
    let state =
      match ICFG.getRetVal cfg with
      | Some value -> addVar state retID value
      | _ -> state
    { state with ICFG = ICFG.addCall state.ICFG instrID cfg }

  let setEdge state src dsts =
    { state with ICFG = ICFG.setEdge state.ICFG src dsts }

  let hasEdge state src dst = ICFG.hasEdge state.ICFG src dst

  let checkEdges state src dsts =
    let set = ICFG.getEdges state.ICFG |> Map.findSet src
    List.filter (fun x -> Set.contains x set) dsts

  let getProg state = state.Prog
  let getDefaultKey state = state.DefaultKey

  let tryFindCache state fID cArgs cfg =
    let key =
      fID, cArgs, ICFG.getEdges cfg, ICFG.getKey cfg, ICFG.getRetConst cfg
    match state.Cache.TryGetValue key with
    | true, ret -> Some ret
    | _ -> None

  let inProcess state fID cArgs cfg =
    let key = fID, cArgs, ICFG.getEdges cfg, ICFG.getKey cfg
    Set.contains key state.InProc

  let getInProc state = state.InProc

  let private prepare func nodes args =
    let evalArg ret = function
      | Def (id, Arg (idx, _)) -> Map.add id (Array.get args idx) ret
      | Alloca (id, _) -> Map.add id (Map.count ret |> Builder.mkLocalAddr) ret
      | _ -> ret

    let evalVar ret = function
      | Def (id, Var (id_, _)) when Map.containsKey id_ ret ->
        Map.add id (Map.find id_ ret) ret
      | _ -> ret

    let evalBB f ret block = Array.fold f ret block
    let body = Function.getBody func
    let body = Set.toArray nodes |> Array.map (Array.get body)
    let vars = Array.fold (evalBB evalArg) Map.empty body
    let vars = Array.fold (evalBB evalVar) vars body

    let updateMem mem addr value =
      match addr with
      | LocalAddr _ ->
        match Map.tryFind addr mem with
        | Some v when v = value -> mem
        | Some v ->
          let undef = Builder.getSize value |> Builder.mkUndef
          Map.add addr undef mem
        | _ -> Map.add addr value mem
      | _ -> mem

    let evalStore mem = function
      | Store (Var (id, _), Num bv) when Map.containsKey id vars ->
        updateMem mem (Map.find id vars) (Num bv)
      | _ -> mem

    let mem = Array.fold (evalBB evalStore) Map.empty body
    vars, mem

  let getVarConst state varID = VarConst.getVar varID state.VarConst

  let getVar_ vars varID =
    match Map.tryFind varID vars with
    | Some (Var (varID, _)) -> varID
    | _ -> varID

  let calcConst state args =
    let calc i = function
      | Arg (idx, _) -> i, VarConst.getArg idx state.VarConst
      | Var (varID, _) -> i, getVarConst state varID
      | _ -> i, None
    Array.mapi calc args |> Array.filter (snd >> Option.isSome) |> Map.ofArray

  let removeIfNot (retExpr, flag) bID ret from value =
    match value with
    | Num bv when (flag && value <> retExpr) || (not flag && value = retExpr) ->
      let prev = Map.findSet from ret
      Map.add from (Set.remove bID prev) ret
    | _ -> ret

  let stripRetConst func vars retNodes edges = function
    | Some retConst ->
      let removeEdge ret (bID, iID) =
        match Array.get (Function.getBB func bID) iID with
        | Phi (_, map, _, _) -> Map.fold (removeIfNot retConst bID) ret map
        | _ -> ret
      let fold ret bID =
        match Function.getBB func bID |> Array.last with
        | Return (Some (Var (varID, _))) -> getVar_ vars varID |> removeEdge ret
        | _ -> ret
      Set.fold fold edges retNodes
    | _ -> edges

  let calcVarConst func vars retConst retNodes edges =
    let fEdges = Function.getEdges func
    let edges = stripRetConst func vars retNodes edges retConst
    let getVar varID = getVar_ vars varID
    let addConst ret varID cond =
      let varID = getVar varID
      let condExpr = if cond then Builder.True else Builder.False
      let ret = VarConst.addVar varID (condExpr, true) ret
      let bID, iID = varID
      if bID >= 0 then
        match Array.get (Function.getBB func bID) iID with
        | Def (_, RelOp (EQ, Var (varID, _), Num bv, _)) ->
          VarConst.addVar varID (Builder.mkNum bv, cond) ret
        | Def (_, RelOp (EQ, Arg (idx, _), Num bv, _)) ->
          VarConst.addArg idx (Builder.mkNum bv, cond) ret
        | Def (_, RelOp (NE, Var (varID, _), Num bv, _)) ->
          VarConst.addVar varID (Builder.mkNum bv, not cond) ret
        | Def (_, RelOp (NE, Arg (idx, _), Num bv, _)) ->
          VarConst.addArg idx (Builder.mkNum bv, not cond) ret
        | _ -> ret
      else ret
    let getConstCond ret from toSet =
      let block = Function.getBB func from
      match Array.last block with
      | CondJmp (Var (varID, _), tID, fID) ->
        if Set.contains tID toSet then addConst ret varID true
        elif Set.contains fID toSet then addConst ret varID false
        else ret
      | _ -> ret
    let fold ret from toSet =
      if (Map.findSet from fEdges |> Set.count) = Set.count toSet then ret
      else getConstCond ret from toSet
    edges, Map.fold fold VarConst.empty edges

  let start state fID cArgs cfg key argsConst retConst =
    let edges = ICFG.getEdges cfg
    let nodes = Function.edgesToNodes edges |> Set.add 0
    let func = Program.getFunc state.Prog fID
    let args = ConstArgs.toArgs func cArgs
    let vars, constMem = prepare func nodes args
    let edges, varConst = calcVarConst func vars retConst cfg.RetNodes edges
    match VarConst.mergeArgs argsConst varConst with
    | Some varConst ->
      let args = VarConst.applyArgs args varConst
      let cfg = ICFG.setEdges edges cfg |> ICFG.fini
      let nodes = Function.edgesToNodes edges |> Set.add 0
      { state with FuncID = fID
                   Edges = edges
                   Args = args
                   Memory = Map.empty
                   ConstMemory = constMem
                   Vars = vars
                   InProc = Set.add key state.InProc
                   RetVal = None, true
                   RetConst = retConst
                   VarConst = varConst
                   ICFG = cfg }
    | _ ->
      { state with FuncID = fID
                   ICFG = ICFG.setEdges Map.empty cfg |> ICFG.fini }

  let addCache state key cfg = state.Cache.TryAdd (key, cfg) |> ignore

  let fini state =
    let cfg = getCFG state |> ICFG.fini
    match state.RetVal with
    | Some value, true -> ICFG.setRetVal cfg value
    | _ -> cfg

  let endAddr = SymExecutor.State.endAddr

module ConstantFolding =
  let private mkBinOp op size e1 e2 = Builder.mkBinOp op e1 e2 size
  let private mkUnOp op size e = Builder.mkUnOp op e size
  let private mkSelect s e1 e2 e3 = Builder.mkSelect e1 e2 e3 s
  let private mkCast op size e = Builder.mkCast op e size
  let private mkExtract size dst pos = Builder.mkExtract dst pos size

  let rec evalExpr state expr =
    match expr with
    | Arg (idx, _) -> State.getArg state idx
    | Num _
    | Undef _
    | UnIntCall _
    | BlockAddr _
    | GlobalVar _
    | LocalAddr _
    | SymVar _
    | Metadata _ -> expr
    | Addr (base_, idx, _) -> evalBin state Builder.mkAddr base_ idx
    | BinOp (op, e1, e2, size, _) -> evalBin state (mkBinOp op size) e1 e2
    | Cast (op, e, size, _) -> evalExpr state e |> mkCast op size
    | Concat (exprs, size, _) -> evalConcat state exprs size
    | Extract (dst, pos, size, _) -> evalBin state (mkExtract size) dst pos
    | Load (addr, size, _) -> State.loadMem state (evalExpr state addr) size
    | RelOp (op, e1, e2, _) -> evalBin state (Builder.mkRelOp op) e1 e2
    | Replace (d, p, s, _, _) -> evalTriple state Builder.mkReplace d p s
    | Select (c, t, f, s, _) -> evalTriple state (mkSelect s) c t f
    | Var (id, _) -> State.getVar state id expr
    | UnOp (op, e, size, _) -> evalExpr state e |> mkUnOp op size
    | Any _ -> failwith "Unreachable"

  and evalBin state conv e1 e2 = conv (evalExpr state e1) (evalExpr state e2)

  and evalTriple state conv e1 e2 e3 =
    let e1 = evalExpr state e1
    let e2 = evalExpr state e2
    let e3 = evalExpr state e3
    conv e1 e2 e3

  and evalConcat state exprs size =
    let exprs = Array.map (evalExpr state) exprs
    Builder.mkConcat exprs size

  let evalExprOpt state = function
    | Some expr -> evalExpr state expr |> Some
    | _ -> None

  let evalStore state addr value =
    State.addMem state (evalExpr state addr) (evalExpr state value)

  let glue iID (state, isEnd, nxts) = state, isEnd, iID + 1, nxts

  let filterDsts state bID dsts = List.filter (State.hasEdge state bID) dsts

  let evalCJmp bID state cond tID fID =
    let cond = evalExpr state cond
    if cond = Builder.True then
      State.removeEdge state bID fID, true, filterDsts state bID [tID]
    elif cond = Builder.False then
      State.removeEdge state bID tID, true, filterDsts state bID [fID]
    else state, true, filterDsts state bID [tID; fID]

  let evalSwitch bID state cond cases dID =
    let cond = evalExpr state cond
    let choose (expr, nxt) =
      if State.hasEdge state bID nxt then Some (Builder.mkEq cond expr, nxt)
      else None
    let isTrue (cond, _) = cond = Builder.True
    let folder nxts (cond, nxt) =
      if cond = Builder.False then nxts else nxt :: nxts
    let cases = Array.choose choose cases
    let nxts =
      match Array.tryFind isTrue cases with
      | Some (_, nxt) -> [nxt]
      | _ -> Array.fold folder (filterDsts state bID [dID]) cases
    Set.ofList nxts |> State.setEdge state bID, true, nxts

  let evalPhi bID state var map sz =
    let choose (src, expr) =
      if State.hasEdge state src bID then evalExpr state expr |> Some
      else None
    match Map.toArray map |> Array.choose choose |> Array.distinct with
    | [|expr|] -> State.addVar state var expr
    | _ ->  Builder.mkUndef sz |> State.addVar state var

  let manageQueue doneSet state nxts =
    let cfg = State.getCFG state
    let rec loop ret = function
      | [] -> ret
      | hd :: remain ->
        if Set.isSubset (ICFG.getFromNodes cfg hd) doneSet then
          hd :: remain @ ret
        else loop (hd :: ret) remain
    loop [] nxts

  let rec evalFunc state addCache fID retID args cArgs cfg =
    let defaultKey = State.getDefaultKey state
    let func = Program.getFunc (State.getProg state) fID
    let body = Function.getBody func
    let isRec = ICFG.isRec cfg
    let retConst = State.getVarConst state retID
    let cfg =
      if isRec then
        Program.getFunc state.Prog fID |> ICFG.ofFunc (ICFG.getKey cfg)
      else cfg
    let cfg = ICFG.setArgs cfg cArgs |> ICFG.setRetConst retConst
    if body = [||] then None, ICFG.dummy fID defaultKey, false
    elif State.inProcess state fID cArgs cfg then
      None, ICFG.dummyRec fID (ICFG.getKey cfg) cArgs retConst, true
    else
      match State.tryFindCache state fID cArgs cfg with
      | Some cfg when addCache -> None, cfg, true
      | _ ->
        let key_ = fID, cArgs, ICFG.getEdges cfg, ICFG.getKey cfg
        let key = fID, cArgs, ICFG.getEdges cfg, ICFG.getKey cfg, retConst
        let argsConst = State.calcConst state args
        let rec loop cfg =
          if ICFG.hasRet cfg then
            let state = State.start state fID cArgs cfg key_ argsConst retConst
            let ret = evalBody body Set.empty state [0] |> State.fini
            if ICFG.isEqual cfg ret then ret
            else loop ret
          else cfg
        let cfg = loop cfg
        if addCache && not isRec then State.addCache state key cfg
        None, cfg, true

  and evalCall state bID iID retID fID args_ nxts =
    let args = Array.map (evalExpr state) args_
    let cArgs = ConstArgs.ofExprs args
    match State.getNxtCFG state (bID, iID) fID cArgs with
    | Some cfg ->
      let state_, cfg, isAdd = evalFunc state true fID retID args_ cArgs cfg
      if ICFG.hasRet cfg then
        let state =
          match state_, isAdd with
          | _ , false -> state
          | Some state, true -> State.addCall state (bID, iID) retID cfg
          | None, true ->
            State.addCall state (bID, iID) retID cfg |> State.flushMem
        state, false, nxts
      else State.removeBlock state bID, true, []
    | _ -> State.removeBlock state bID, true, []

  and evalBody body doneSet state = function
    | bID :: remain ->
      if Set.contains bID doneSet then evalBody body doneSet state remain
      else
        let state, nxts = Array.get body bID |> evalBB state bID
        let doneSet = Set.add bID doneSet
        manageQueue doneSet state (nxts @ remain)
        |> evalBody body doneSet state
    | _ -> state

  and evalBB state bID block =
    match Array.last block with
    | UnReachable -> State.removeBlock state bID, []
    | _ ->
      let state = State.flushMem state
      let state, _, _, nxts =
        Array.fold (evalStmt bID) (state, false, 0, []) block
      state, State.checkEdges state bID nxts

  and evalStmt bID (state, isEnd, iID, nxts) stmt =
    if isEnd then state, isEnd, iID + 1, nxts
    else
      match stmt with
      | Def (var, e) -> evalExpr state e |> State.addVar state var, false, nxts
      | Call (retID, Callee.ID fID, args, _, _) ->
        evalCall state bID iID retID fID args nxts
      | Try (retID, Callee.ID fID, args, _, nxt, _, _) ->
        evalCall state bID iID retID fID args [nxt]
      | Call _
      | Try _ -> state, false, nxts
      | Throw _ -> State.removeBlock state bID, true, []
      | Jmp nxt -> state, true, [nxt]
      | IndJmp (_, targets) -> state, false, Array.toList targets
      | CondJmp (cond, tID, fID) -> evalCJmp bID state cond tID fID
      | Switch (cond, cases, dID) -> evalSwitch bID state cond cases dID
      | Return e -> evalExprOpt state e |> State.doReturn state bID, true, []
      | Store (addr, value) -> evalStore state addr value, false, nxts
      | Phi (var, map, sz, _) -> evalPhi bID state var map sz, false, nxts
      | Alloca _
      | Catch _
      | NOP -> state, false, nxts
      | UnReachable -> State.removeBlock state bID, true, []
      |> glue iID

  let start state fID cfg =
    let _, cfg, _ = evalFunc state false fID State.endAddr [||] ConstArgs.empty cfg
    cfg
