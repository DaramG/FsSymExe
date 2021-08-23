namespace SymExecutor

open LLIR
open LLIR.Core
open Common

type State<'T> = {
  Prog: Program

  Fnames: ID list
  Funcs: Function list

  Memory: Memory
  VarState: Map<VarID, Expr> list
  Args: Expr [] list
  RetVal: Expr option

  InstrID: int
  RetID: VarID list
  RetAddr: (BlockID * int) list
  Paths: BlockID list list
  PC: PathConstraint
  PathCnt: Map<BlockID * BlockID, int> list
  CallCnt: Map<ID * BlockID * int * ID, int>
  UnIntMap: Map<ID, int>

  (* User defined *)
  UserState: 'T
  IsReachable: State<'T> -> BlockID -> int -> State<'T> option
  GetSymFunc: State<'T> -> ID -> SymFunc_<'T> option
  StmtCb: State<'T> -> int -> State<'T>
  CallCb: State<'T> -> int -> ID -> State<'T>
  LoadCb: State<'T> -> Expr -> ExprSize -> Expr -> State<'T>
  StoreCb: State<'T> -> Expr -> Expr -> State<'T>
  ReturnCb: State<'T> -> Expr option -> State<'T>
  UseSAT: bool
}

(* State -> InstrID -> RetID -> Args -> RetSize -> State option*)
and SymFunc_<'T> = State<'T> -> int -> VarID -> Expr [] -> int -> State<'T> option

and SymFuncTbl<'T> = Map<ID, SymFunc_<'T>>

module State =
  let endAddr = -1, -1
  let private maxLoop = 2

  let getCurFname state =
    match state.Fnames with
    | [] -> ""
    | fname :: _ -> fname

  let getCurFunc state = List.head state.Funcs

  let inStartFunc state =
    match state.Funcs with
    | [_] -> true
    | _ -> false

  let getLastBB state = PathConstraint.getCurBB state.PC

  let getCurBB state =
    match List.head state.Paths with
    | [] -> -1
    | bb :: _ -> bb

  let getCurPos state = getCurBB state, state.InstrID

  let getPrevBB state =
    match List.head state.Paths with
    | _ :: bb :: _ -> bb
    | _ -> failwith "getPrevBB fail"

  let getPad state = String.replicate (List.length state.Paths) "  "

  let private incCnt f t map =
    let key = f, t
    match Map.tryFind key map with
    | Some v -> Map.add key (v + 1) map
    | _ -> Map.add key 1 map

  let private incCallCnt f map =
    match Map.tryFind f map with
    | Some v -> Map.add f (v + 1) map
    | _ -> Map.add f 1 map

  let private decCallCnt f map =
    match Map.tryFind f map with
    | Some v -> Map.add f (v - 1) map
    | _ -> failwith "err"

  (* User defined *)
  let getPathCnt state bID =
    let key = getCurBB state, bID
    match Map.tryFind key (List.head state.PathCnt) with
    | Some cnt -> cnt
    | _ -> 0

  let checkPathCnt maxLoop state bID iID =
    if iID = 0 then getPathCnt state bID < maxLoop
    else true

  let private nopGetSymFunc _ _ = None
  let private nopLoadCb state _ _ _ = state
  let private nopStoreCb state _ _ = state
  let private nopReturnCb state _ = state
  let private nopStmtCb state _ = state
  let private nopCallCb state _ _ = state

  let private basicIsReachable state bID iID =
    if checkPathCnt maxLoop state bID iID then Some state
    else None

  let setIsReachable f state = { state with IsReachable = f }
  let setGetSymFunc f state = { state with GetSymFunc = f }
  let setLoadCb f state = { state with LoadCb = f }
  let setStoreCb f state = { state with StoreCb = f }
  let setReturnCb f state = { state with ReturnCb = f }
  let setUseSAT v state = { state with UseSAT = v }
  let setStmtCb f state = { state with StmtCb = f }
  let setCallCb f state = { state with CallCb = f }

  let getUserState state = state.UserState
  let setUserState ustate state = { state with UserState = ustate }

  let init prog ustate = {
    Prog = prog
    Fnames = []
    Funcs = []

    Memory = Memory.init prog
    VarState = []
    Args = []
    RetVal = None

    InstrID = -1
    RetID = []
    RetAddr = []
    Paths = []
    PC = PathConstraint.empty ()
    PathCnt = []
    CallCnt = Map.empty
    UnIntMap = Map.empty

    UserState = ustate
    IsReachable = basicIsReachable
    GetSymFunc = nopGetSymFunc
    StmtCb = nopStmtCb
    CallCb = nopCallCb
    LoadCb = nopLoadCb
    StoreCb = nopStoreCb
    ReturnCb = nopReturnCb
    UseSAT = true
  }

  let start state = { state with PC = PathConstraint.empty () }

  let getCallKey frID (rbID, riID) toID = frID, rbID, riID - 1, toID

  let initFunc state fID retID retAddr args =
    let func = Program.getFunc state.Prog fID
    let curID = getCurFname state
    let callCnt = incCallCnt (getCallKey curID retAddr fID) state.CallCnt
    { state with Args = args :: state.Args
                 VarState = Map.empty :: state.VarState
                 PathCnt = Map.empty :: state.PathCnt
                 Paths = [] :: state.Paths
                 Fnames = fID :: state.Fnames
                 RetID = retID :: state.RetID
                 RetAddr = retAddr :: state.RetAddr
                 Funcs = func :: state.Funcs
                 RetVal = None
                 CallCnt = callCnt
                 Memory = Memory.initFunc state.Memory }

  let prepare state fID ustate =
    let func = Program.getFunc state.Prog fID
    Function.getArgs func |> Array.map (Expr.Arg)
    |> initFunc state fID endAddr endAddr
    |> setUserState ustate

  let setArg idx expr state =
    Array.set (List.head state.Args) idx expr
    state

  let debug state =
    let printVar id expr =
      printfn "%s = %s" (Pp.ppVarID id) (Pp.printExpr expr)
    let printVars map =
      printfn "-------------------------------------"
      Map.iter printVar map
    let printArgs args =
      printfn "-------------------------------------"
      Array.iteri (fun i arg -> printfn "[%d] = %s" i (Pp.printExpr arg)) args
    let printPath fname paths =
      printfn "%s:" fname
      List.iter (printfn "%d") paths
    printfn "Args"
    List.iter printArgs state.Args
    printfn "Funcs"
    List.iter (printfn "%s") state.Fnames
    Pp.printExprOpt state.RetVal |> printfn "Return Value: %s"
    printfn "VarStates"
    List.iter printVars state.VarState
    printfn "MemState"
    Memory.debug state.Memory
    PathConstraint.debug state.PC

  (* Variable related funcs *)

  let getArg state idx = Array.get (List.head state.Args) idx

  let getVar state id = List.head state.VarState |> Map.find id

  let addVar id state value =
    let vars = List.head state.VarState |> Map.add id value
    { state with VarState = vars :: List.tail state.VarState }

  let private addVar_ id value state = addVar id state value

  let private addVarOpt id value state =
    match value with
    | Some value -> addVar id state value
    | _ -> state

  (* Memory related funcs *)

  let alloc id state size =
    match size with
    | Num bv ->
      let size = BitVector.toInt bv
      let addr, mem = Memory.alloc state.Memory size
      let state = addVar id state addr
      { state with Memory = mem }
    | _ -> failwith "TODO: State.alloc"

  let rawLoad size state addr =
    let mem, value = Memory.load state.Memory addr size
    { state with Memory = mem }, value

  let load size state addr =
    let mem, value = Memory.load state.Memory addr size
    state.LoadCb { state with Memory = mem } addr size value, value

  let rawStore state addr value =
    { state with Memory = Memory.store state.Memory addr value }

  let store state addr value =
    let state = { state with Memory = Memory.store state.Memory addr value }
    state.StoreCb state addr value

  (* Control flow related funcs *)

  let addBB state bID =
    let fname = getCurFname state
    let paths = state.Paths
    let pathCnt = List.head state.PathCnt |> incCnt (getCurBB state) bID
    { state with Paths = (bID :: List.head paths) :: List.tail paths
                 PC = PathConstraint.addBB state.PC fname bID
                 PathCnt = pathCnt :: List.tail state.PathCnt }

  let isReachable state bID iID =
    if bID = -1 then None
    else state.IsReachable state bID iID

  let isCallable state fID retAddr =
    let curID = getCurFname state
    match Map.tryFind (getCallKey curID retAddr fID) state.CallCnt with
    | Some cnt when cnt >= maxLoop -> false
    | _ -> true

  (* PathConstraint *)

  let isSAT solver state cond =
    if cond = Builder.True then Some state, solver
    elif cond = Builder.False then None, solver
    elif state.UseSAT then
      match PathConstraint.isSAT solver state.PC cond with
      | solver, pc, SAT -> Some { state with PC = pc }, solver
      | solver, _, UNSAT -> None, solver
      | solver, _, TIMEOUT -> Some state, solver
    else Some state, solver

  let getPcPath state = state.PC.Paths
  let getPC state = state.PC
  let getPcConds state = state.PC.Conds
  let getLastZ3Cond state = PathConstraint.getLastZ3Cond state.PC

  (* Others *)

  let setRetVal (state: State<'T>) ret = { state with RetVal = ret }

  let doReturn state ret =
    let fID = getCurFname state
    let ffID =
      match state.Fnames with
      | _ :: ffID :: _ -> ffID
      | _ -> ""
    let retAddr = List.head state.RetAddr
    let state_ =
      { state with Fnames = List.tail state.Fnames
                   RetID = List.tail state.RetID
                   RetAddr = List.tail state.RetAddr
                   Funcs = List.tail state.Funcs
                   VarState = List.tail state.VarState
                   Args = List.tail state.Args
                   Paths = List.tail state.Paths
                   PathCnt = List.tail state.PathCnt
                   CallCnt = decCallCnt (getCallKey ffID retAddr fID) state.CallCnt
                   Memory = Memory.doReturn state.Memory
      }
    if retAddr = endAddr then setRetVal state_ ret, fID, retAddr
    else addVarOpt (List.head state.RetID) ret state_, fID, retAddr

  let getBlock state bID = Function.getBB (getCurFunc state) bID

  let private getUnIntCnt map fID =
    match Map.tryFind fID map with
    | Some cnt -> cnt, Map.add fID (cnt + 1) map
    | _ -> 0, Map.add fID 1 map

  let mkUnIntCall state fID args size =
    let cnt, map = getUnIntCnt state.UnIntMap fID
    let fname = sprintf "UnInt_%s_%d" fID cnt
    { state with UnIntMap = map }, Builder.mkUnIntCall fname args size

  let mkBlockCmp state target bID =
   Builder.mkBlockAddr (getCurFname state, bID) |> Builder.mkEq target

  let isDeclr state fID = Program.getFunc state.Prog fID |> Function.isDeclr

  let setInstrID state iID = { state with InstrID = iID }
  let getInstrID state = state.InstrID

  let getRetAddr state = state.RetAddr
  let getPaths state = state.Paths

  let stmtCb state iID = state.StmtCb state iID
  let callCb state iID fID = state.CallCb state iID fID

  let setProg prog state =
    let funcs = Program.getFuncs prog
    { state with Prog = prog
                 Funcs = List.map (fun x -> Map.find x funcs ) state.Fnames }
