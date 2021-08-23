namespace SymExecutor

open LLIR
open Common

module Executor =
  let private (=>) (a, b) f = a, f b
  let private mkBinOp op size e1 e2 = Builder.mkBinOp op e1 e2 size
  let private mkUnOp op size e = Builder.mkUnOp op e size
  let private mkSelect s e1 e2 e3 = Builder.mkSelect e1 e2 e3 s
  let private mkCast op size e = Builder.mkCast op e size
  let private mkExtract size dst pos = Builder.mkExtract dst pos size

  let rec execExpr state expr =
    match expr with
    | Addr (b, i, _) -> execBin state Builder.mkAddr  b i
    | Arg (idx, _) -> state, State.getArg state idx
    | Num _
    | Undef _
    | BlockAddr _
    | GlobalVar _
    | LocalAddr _
    | SymVar _
    | Metadata _ -> state, expr
    | UnIntCall (fID, args, size, _) -> execUnIntCall state fID args size
    | BinOp (op, e1, e2, size, _) -> execBin state (mkBinOp op size) e1 e2
    | Cast (op, e, size, _) -> execExpr state e => mkCast op size
    | Concat (exprs, size, _) -> execConcat state exprs size
    | Extract (dst, pos, size, _) -> execBin state (mkExtract size) dst pos
    | Load (addr, size, _) -> execExpr state addr ||> State.load size
    | RelOp (op, e1, e2, _) -> execBin state (Builder.mkRelOp op) e1 e2
    | Replace (d, p, s, _, _) -> execTriple state Builder.mkReplace d p s
    | Select (c, t, f, s, _) -> execTriple state (mkSelect s) c t f
    | Var (id, _) -> state, State.getVar state id
    | UnOp (op, e, size, _) -> execExpr state e => (mkUnOp op size)
    | Any _ -> failwith "Unreachable"

  and execUnIntCall state fID args size =
    let state, args = execExprs state args
    state, Builder.mkUnIntCall fID args size

  and execBin state conv e1 e2 =
    let state, e1 = execExpr state e1
    let state, e2 = execExpr state e2
    state, conv e1 e2

  and execExprsFold (state, ret) expr =
    let state, expr = execExpr state expr
    state, expr :: ret

  and execExprs state exprs =
    Array.fold execExprsFold (state, []) exprs => Array.revList

  and execConcat state exprs size =
    let state, exprs = execExprs state exprs
    state, Builder.mkConcat exprs size

  and execTriple state conv e1 e2 e3 =
    let state, e1 = execExpr state e1
    let state, e2 = execExpr state e2
    let state, e3 = execExpr state e3
    state, conv e1 e2 e3

  let execExprOpt state = function
    | Some expr -> execExpr state expr => Some
    | _ -> state, None

  let execCallee state = function
    | Callee.ID id -> state, Some id
    | Callee.Expr expr ->
      let state, expr = execExpr state expr
      // TODO: expr
      state, None
    | Callee.InlineAsm _-> state, None

  let execDef mgr state id expr =
    let state, expr = execExpr state expr
    mgr, State.addVar id state expr |> Some

  let execAlloc mgr state id expr =
    let state, expr = execExpr state expr
    mgr, State.alloc id state expr |> Some

  let execPhi mgr state id map =
    let state, value = Map.find (State.getPrevBB state) map |> execExpr state
    mgr, State.addVar id state value |> Some

  let execStore mgr state addr value =
    let state, addr = execExpr state addr
    let state, value = execExpr state value
    mgr, State.store state addr value |> Some

  let execCatch mgr state id size =
    // TODO
    mgr, Builder.mkUndef size |> State.addVar id state |> Some

  let execSwitch mgr state cond cases default_ =
    let state, cond = execExpr state cond
    let fold (tasks, conds) (cc, nxt) =
      let state, cc = execExpr state cc
      let cond = Builder.mkEq cond cc
      let conds = Builder.mkOr cond conds 1
      (state, cond, nxt, 0) :: tasks, conds
    let tasks, conds = Array.fold fold ([], Builder.False) cases
    let tasks = (state, Builder.mkNot conds, default_, 0) :: tasks
    Manager.addTasks mgr state tasks, None

  let execCondJmp mgr state cond tID fID =
    let state, cond = execExpr state cond
    let notCond = Builder.mkNot cond
    let tasks = [(state, cond, tID, 0); (state, notCond, fID, 0)]
    Manager.addTasks mgr state tasks, None

  let execThrow mgr state expr =
    // TODO
    let state, expr = execExpr state expr
    mgr, Some state

  let execIndJmp mgr state target cands =
    let state, target = execExpr state target
    let map bID =
      let cond = State.mkBlockCmp state target bID
      state, cond, bID, 0
    let tasks = Array.map map cands |> Array.toList
    Manager.addTasks mgr state tasks, None

  let execFunc mgr state fID iID retID retAddr args size =
    //Pp.printExprs args |> Logger.info "Call: %s %s" fID
    match SymFunc.tryFind state fID with
    | Some func -> mgr, func state iID retID args size
    | _ ->
      if State.isCallable state fID retAddr then
        let state = State.callCb state iID fID
        let state = State.initFunc state fID retID retAddr args
        Manager.addTrueTask mgr state 0 0, None
      else mgr, None

  let execCall mgr state iID retAddr id callee args size =
    let state, callee = execCallee state callee
    let state, args = execExprs state args
    match callee with
    | Some fID -> execFunc mgr state fID iID id retAddr args size
    | _ ->
      mgr,
      State.mkUnIntCall state "unknown" args size ||> State.addVar id |> Some

  let execTry mgr state iID id callee args size nID eID =
    // TODO
    execCall mgr state iID (nID, 0) id callee args size

  let execReturn mgr state exprOpt =
    let state, exprOpt = execExprOpt state exprOpt
    let state = state.ReturnCb state exprOpt
    let state, bfID, (bID, iID) = State.doReturn state exprOpt
    Manager.addTrueTask mgr state bID iID, None

  let execStmt mgr state iID stmt =
    let state = State.setInstrID state iID
    let state = State.stmtCb state iID
    //printf "%s|-%s" (State.getPad state) (Pp.printStmt 0 stmt)
    match stmt with
    | Def (id, expr) -> execDef mgr state id expr
    | Alloca (id, expr) -> execAlloc mgr state id expr
    | Phi (id, map, _, _) -> execPhi mgr state id map
    | Store (addr, value) -> execStore mgr state addr value
    | Catch (id, size) -> execCatch mgr state id size
    | Try (id, callee, args, size, nID, eID, _) ->
      execTry mgr state iID id callee args size nID eID
    | Call (id, callee, args, size, _) ->
      execCall mgr state iID (State.getCurBB state,iID + 1) id callee args size
    | Switch (cond, cases, default_) -> execSwitch mgr state cond cases default_
    | Throw (expr, _) -> execThrow mgr state expr
    | IndJmp (target, cands) -> execIndJmp mgr state target cands
    | CondJmp (cond, tID, fID) -> execCondJmp mgr state cond tID fID
    | Jmp bID -> Manager.addTrueTask mgr state bID 0, None
    | Return exprOpt -> execReturn mgr state exprOpt
    | NOP -> mgr, Some state
    | UnReachable -> mgr, None

  let execBB mgr state bID iID =
    //Logger.info "%s|- %s, %d, %d" (State.getPad state) (State.getCurFname state) bID iID
    let state = if iID = 0 then State.addBB state bID else state
    let stmts = State.getBlock state bID
    let length = Array.length stmts
    let rec loop mgr state iID =
      if iID < length then
        match Array.get stmts iID |> execStmt mgr state iID with
        | mgr, Some state -> loop mgr state (iID + 1)
        | mgr, None -> mgr
      else mgr
    loop mgr state iID

  let start mgr =
    let rec loop mgr =
      if Manager.isEnd mgr then mgr
      else
        let mgr, (state, cond, bID, iID) = Manager.pop mgr
        execBB mgr state bID iID |> loop
    loop mgr |> Manager.reset

