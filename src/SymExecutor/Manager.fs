namespace SymExecutor

open LLIR
open System
open Common

exception TimeEndException

type SymTask<'T> = State<'T> * Expr * BlockID * int
type SymTasks<'T> = SymTask<'T> list

type AddTaskCode =
  | Add
  | End
  | UnSAT
  | Unreachable

type Manager<'T> = {
  Tasks: SymTasks<'T>
  DoneStates: State<'T> list
  Timer: Diagnostics.Stopwatch
  ScheduleTasks: State<'T> -> SymTasks<'T> -> SymTasks<'T>
  Reschedule: Manager<'T> -> (AddTaskCode * SymTask<'T>) list -> Manager<'T>
  Timeout: int64
  Solver: Solver option
  CondsInSolver: Expr list
}

module AddTaskCode =
  let isAdd = function
    | Add -> true
    | _ -> false

  let isUnSAT = function
    | UnSAT -> true
    | _ -> false

module Manager =

  let private start () =
    let timer = new Diagnostics.Stopwatch ()
    timer.Start ()
    timer

  let init state timeout = {
    Tasks = [State.start state, Builder.True, 0, 0]
    Timer = start ()
    DoneStates = []
    ScheduleTasks = (fun _ tasks -> tasks)
    Reschedule = (fun mgr _ -> mgr)
    Timeout = int64 timeout
    Solver = None
    CondsInSolver = []
  }

  let setScheduleTasks f mgr = { mgr with ScheduleTasks = f }
  let setReschedule f mgr = { mgr with Reschedule = f }

  let reset mgr =
    match mgr.Solver with
    | Some solver -> solver.Reset ()
    | _ -> ()

  let private checkTimeout mgr =
    if mgr.Timer.ElapsedMilliseconds > mgr.Timeout && mgr.Timeout > 0L then
      reset mgr
      raise TimeEndException
    else ()

  let private _addTask mgr task = { mgr with Tasks = task :: mgr.Tasks }
  let private addTaskBack mgr task = { mgr with Tasks = mgr.Tasks @ [task] }

  let private addTask mgr solver state cond bID iID =
    if (bID, iID) = State.endAddr then
      End, { mgr with DoneStates = state :: mgr.DoneStates }, solver
    else
      match State.isReachable state bID iID with
      | Some state ->
        checkTimeout mgr
        match State.isSAT solver state cond with
        | Some state, solver ->
          if State.checkPathCnt 100 state bID iID then
            Add, _addTask mgr (state, cond, bID, iID), solver
          else
            Add, addTaskBack mgr (state, cond, bID, iID), solver
        | _, solver -> UnSAT, mgr, solver
      | _ -> Unreachable, mgr, solver

  let addTrueTask mgr state bID iID =
    if (bID, iID) = State.endAddr then
      { mgr with DoneStates = state :: mgr.DoneStates }
    else
      match State.isReachable state bID iID with
      | Some state -> _addTask mgr (state, Builder.True, bID, iID)
      | _ -> mgr

  let getSolver mgr conds =
    match mgr.Solver with
    | Some solver when conds = mgr.CondsInSolver -> Some solver
    | _ -> None

  let setSolver mgr solver conds =
    { mgr with Solver = solver; CondsInSolver = conds }

  let addTasks mgr state tasks =
    let tasks = mgr.ScheduleTasks state tasks
    let conds = State.getPcConds state
    let solver = getSolver mgr conds
    let folder task (ret, mgr, solver) =
      let state, cond, bID, iID = task
      let retCode, mgr, solver = addTask mgr solver state cond bID iID
      (*
      let fname = State.getCurFname state
      let cbID = State.getCurBB state
      Logger.info "%s %d -> (%d, %d) %A" fname cbID bID iID retCode
      *)
      (retCode, task) :: ret, mgr, solver
    let ret, mgr, solver = List.foldBack folder tasks ([], mgr, solver)
    let mgr = setSolver mgr solver conds
    mgr.Reschedule mgr ret

  let pop mgr =
    let task = List.head mgr.Tasks
    let state, _, _, _ = task
    let conds = State.getPcConds state
    let conds, solver =
      match conds, mgr.Solver with
      | _ :: prev, Some solver when prev = mgr.CondsInSolver ->
        let cond = State.getLastZ3Cond state
        solver.Assert (cond)
        conds, Some solver
      | conds, Some solver when conds = mgr.CondsInSolver -> conds, Some solver
      | _, Some solver ->
        solver.Reset ()
        [], None
      | _ -> [], None

    { mgr with Tasks = List.tail mgr.Tasks
               Solver = solver
               CondsInSolver = conds }, task

  let isEnd mgr =
    checkTimeout mgr
    List.isEmpty mgr.Tasks

  let setTasks mgr tasks = { mgr with Tasks = tasks }

  let getDoneStates mgr = mgr.DoneStates

  let private getBackPC (code, task) =
    if AddTaskCode.isUnSAT code then
      let state, cond, _, _ = task
      PathConstraint.subSAT (State.getPC state) cond
    else None

  let findSubSatTasks (state, cond, _, _) tasks =
    match PathConstraint.subSAT (State.getPC state) cond with
    | Some pc ->
      let finder (state, _, _, _) =
        let pc_ = State.getPC state
        PathConstraint.isSamePath pc_ pc
      List.partition finder tasks
    | _ -> [], tasks
