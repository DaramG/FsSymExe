namespace LLIR

exception Z3Exception of string

open System.Collections
open Microsoft
open LLIR
open Common

type Z3Result =
  | SAT
  | UNSAT
  | TIMEOUT

type Z3Ctx = Microsoft.Z3.Context

type Z3Mem = Map<Expr, Z3.ArrayExpr>
type Z3RawMem = Map<Expr * uint32, Z3.BitVecExpr>
type Solver = Z3.Solver

type Z3State = {
  True: Z3.BitVecExpr
  False: Z3.BitVecExpr
  IntTy: Z3.Sort
  ByteTy: Z3.Sort
  Mem: Z3Mem
  RawMem: Z3RawMem
  Ctx: Z3Ctx
  UnknownCnt: int
  Depth: int
}
module Z3State =
  let init (ctx: Z3Ctx) = {
    True = ctx.MkBV (1, 1u)
    False = ctx.MkBV (0, 1u)
    IntTy = ctx.MkIntSort ()
    ByteTy = ctx.MkBitVecSort (8u)
    Mem = Map.empty
    RawMem = Map.empty
    Ctx = ctx
    UnknownCnt = 0
    Depth = 0
  }

  let memGetOrAdd state (ctx: Z3Ctx) addr =
    match Map.tryFind addr state.Mem with
    | Some expr -> state, expr
    | None ->
      let name = Map.count state.Mem |> sprintf ".m%d"
      let expr = ctx.MkArrayConst (name, state.IntTy, state.ByteTy)
      { state with Mem = Map.add addr expr state.Mem }, expr

  let rawMemGetOrAdd state (ctx: Z3Ctx) addr size =
    match Map.tryFind (addr, size) state.RawMem with
    | Some expr -> state, expr
    | None ->
      let name = Map.count state.RawMem |> sprintf ".rm%d"
      let expr = ctx.MkBVConst (name, size)
      { state with RawMem = Map.add (addr, size) expr state.RawMem }, expr

  let mkUnknown state =
    let cnt = state.UnknownCnt
    let name = sprintf ".x%d" cnt
    { state with UnknownCnt = cnt + 1 }, name

  let resetDepth (state: Z3State) = { state with Depth = 0 }
  let incDepth (state: Z3State) =
    let depth = state.Depth
    if depth > 10000 then
      Z3Exception "Too Big" |> raise
    else { state with Depth = state.Depth + 1 }

module Z3 =

  let private (=>) (a, b) f = a, f b

  let private err str = Z3Exception str |> raise

  let inline mkIsTrue (state: Z3State) (ctx: Z3Ctx) e = ctx.MkEq (e, state.True)
  let inline mkBVNot (ctx: Z3Ctx) e = ctx.MkBVNot (e)
  let inline mkBVNeg (ctx: Z3Ctx) e = ctx.MkBVNeg (e)

  let addVar state (ctx: Z3Ctx) sz (id: string) =
    state, ctx.MkBVConst (id, uint32 sz)

  let bvToZ3 (ctx: Z3Ctx) bv =
    let len = BitVector.getLength bv |> uint32
    if len > 64u then ctx.MkBV ((BitVector.getBigNum bv).ToString(), len)
    else ctx.MkBV (BitVector.getNum bv, len)
    :> Z3.BitVecExpr

  let rec exprToZ3 (state: Z3State) (ctx: Z3Ctx) expr =
    let state = Z3State.incDepth state
    match expr with
    | Any _
    | Metadata _ -> err "anyToZ3"
    | Addr (b, i, _) -> binToZ3 state ctx ADD b i
    | Arg (id, size) -> sprintf ".a%d" id |> addVar state ctx size
    | BinOp (op, e1, e2, _, _) -> binToZ3 state ctx op e1 e2
    | BlockAddr _ -> err "blockAddrToZ3"
    | Cast (op, e, size, _) -> castToZ3 state ctx op e size
    | Concat (exprs, _, _) -> concatToZ3 state ctx exprs
    | Extract (e, pos, size, _) -> extractToZ3 state ctx e pos size
    | Num bv -> state, bvToZ3 ctx bv
    | Load (addr, size, _) -> loadToZ3 state ctx addr size
    | LocalAddr idx -> sprintf ".l%d" idx |> addVar state ctx Builder.ptrSizeInt
    | RelOp (op, e1, e2, _) ->
      let state, cond = relToZ3 state ctx op e1 e2
      state, ctx.MkITE (cond, state.True, state.False) :?> Z3.BitVecExpr
    | Replace (dst, pos, src, _, _) -> replaceToZ3 state ctx dst pos src
    | Select (c, t, f, _, _) -> selectToZ3 state ctx c t f
    | SymVar (id, size) -> sprintf ".sym.%s" id |> addVar state ctx size
    | Var _ -> err "varToZ3"
    | GlobalVar (id, size) -> addVar state ctx size id
    | Undef (id, size) -> sprintf ".u%d" id |> addVar state ctx size
    | UnIntCall (func, args, size, _) -> unIntCallToZ3 state ctx func args size
    | UnOp (NOT, e, _, _) -> exprToZ3 state ctx e => (mkBVNot ctx)
    | UnOp (NEG, e, _, _) -> exprToZ3 state ctx e => (mkBVNeg ctx)

  and binToZ3 state ctx op e1 e2 =
    let state, e1 = exprToZ3 state ctx e1
    let state, e2 = exprToZ3 state ctx e2
    let ret =
      match op with
      | ADD  -> ctx.MkBVAdd (e1, e2)
      | SUB  -> ctx.MkBVSub (e1, e2)
      | MUL  -> ctx.MkBVMul (e1, e2)
      | UDIV -> ctx.MkBVUDiv (e1, e2)
      | SDIV -> ctx.MkBVSDiv (e1, e2)
      | UREM -> ctx.MkBVURem (e1, e2)
      | SREM -> ctx.MkBVSRem (e1, e2)
      | AND  -> ctx.MkBVAND (e1, e2)
      | OR   -> ctx.MkBVOR (e1, e2)
      | XOR  -> ctx.MkBVXOR (e1, e2)
      | SHL  -> ctx.MkBVSHL (e1, e2)
      | LSHR -> ctx.MkBVLSHR (e1, e2)
      | ASHR -> ctx.MkBVASHR (e1, e2)
    state, ret

  and castToZ3 state ctx op e size =
    let size = uint32 size
    let state, e = exprToZ3 state ctx e
    let ret =
      match op with
      | ZEXT -> ctx.MkZeroExt (size - e.SortSize, e)
      | SEXT -> ctx.MkSignExt (size - e.SortSize, e)
    state, ret

  and concatToZ3 state ctx exprs =
    let folder (state, ret) expr =
      let state, expr = exprToZ3 state ctx expr
      state, ctx.MkConcat (ret, expr)
    let expr, exprs = Array.pop exprs
    Array.fold folder (exprToZ3 state ctx expr) exprs

  and extractToZ3 state ctx e pos size =
    let eSize = Builder.getSize e |> uint32
    let pSize = Builder.getSize pos |> uint32
    let state, e = exprToZ3 state ctx e
    let state, pos = exprToZ3 state ctx pos
    let pos =
      if eSize = pSize then pos
      elif eSize < pSize then ctx.MkExtract (eSize - 1u, 0u, pos)
      else ctx.MkZeroExt (eSize - pSize, pos)
    state, ctx.MkExtract (uint32 size - 1u, 0u, ctx.MkBVLSHR (e, pos))

  and relToZ3 state ctx op e1 e2 =
    let state, e1 = exprToZ3 state ctx e1
    let state, e2 = exprToZ3 state ctx e2
    let ret =
      match op with
      | EQ  -> ctx.MkEq (e1, e2)
      | NE  -> ctx.MkNot (ctx.MkEq (e1, e2))
      | ULT -> ctx.MkBVULT (e1, e2)
      | ULE -> ctx.MkBVULE (e1, e2)
      | UGT -> ctx.MkBVUGT (e1, e2)
      | UGE -> ctx.MkBVUGE (e1, e2)
      | SLT -> ctx.MkBVSLT (e1, e2)
      | SLE -> ctx.MkBVSLE (e1, e2)
      | SGT -> ctx.MkBVSGT (e1, e2)
      | SGE -> ctx.MkBVSGE (e1, e2)
    state, ret

  and replaceToZ3 state ctx dst_ pos_ src_ =
    let dSize_ = Builder.getSize dst_
    let dSize = uint32 dSize_
    let sSize = Builder.getSize src_ |> uint32
    let state, dst = exprToZ3 state ctx dst_
    let state, pos = exprToZ3 state ctx pos_
    let state, src = exprToZ3 state ctx src_
    if dSize = sSize then state, src
    else
      match pos_ with
      | Num bv ->
        let pos = BitVector.toInt bv |> uint32
        let ret =
          if pos = 0u then src
          else ctx.MkConcat (ctx.MkExtract (pos - 1u, 0u, dst), src)
        let ret =
          if pos + sSize = dSize then ret
          else ctx.MkConcat (ret, ctx.MkExtract(dSize - 1u, pos + sSize, dst))
        state, ret
      | _ ->
        let state, name = Z3State.mkUnknown state
        addVar state ctx dSize_ name

  and selectToZ3 state ctx c t f =
    let state, t = exprToZ3 state ctx t
    let state, f = exprToZ3 state ctx f
    let state, c =
      match c with
      | RelOp (op, e1, e2, _) -> relToZ3 state ctx op e1 e2
      | _ -> exprToZ3 state ctx c => (mkIsTrue state ctx)
    state, ctx.MkITE (c, t, f) :?> Z3.BitVecExpr

  and loadToZ3 state ctx addr size =
    let size = uint32 size
    match addr with
    | Addr (addr, pos, _) ->
      let state, pos = exprToZ3 state ctx pos
      let pSize = pos.SortSize
      let state, mem = Z3State.memGetOrAdd state ctx addr
      let rec loop (idx: uint32) size =
        let pos = ctx.MkBV2Int (ctx.MkBVAdd (pos, ctx.MkBV (idx, pSize)), true)
        let value = ctx.MkSelect (mem, pos) :?> Z3.BitVecExpr
        if size = 8u then value
        elif size > 8u then ctx.MkConcat (value, loop (idx + 1u) (size - 8u))
        else ctx.MkExtract (size - 1u, 0u, value)
      state, loop 0u size
    | addr -> Z3State.rawMemGetOrAdd state ctx addr size

  and exprsToZ3 state ctx exprs =
    let folder (state, ret) expr =
      let state, expr = exprToZ3 state ctx expr
      state, (expr :> Z3.Expr) :: ret
    Array.fold folder (state, []) exprs => Array.revList

  and unIntCallToZ3 state ctx func args size =
    let mkBVType size = ctx.MkBitVecSort (uint32 size) :> Z3.Sort
    let argsTy = Array.map (Builder.getSize >> mkBVType) args
    let state, args = exprsToZ3 state ctx args
    let retTy = mkBVType size
    let func = ctx.MkFuncDecl (func, argsTy, retTy)
    state, ctx.MkApp (func, args) :?> Z3.BitVecExpr

  let z3ToExpr state z3Expr = failwith "todo"

  let exprToZ3Cond state expr =
    let ctx = state.Ctx
    let state, expr = exprToZ3 (Z3State.resetDepth state) ctx expr
    state, ctx.MkEq (expr, state.True)

  let simplify expr =
    use ctx = new Microsoft.Z3.Context ()
    let state = Z3State.init ctx
    let state, z3 = exprToZ3 state ctx expr
    z3.Simplify () |> z3ToExpr state

  let getSolver state conds =
    let solver = state.Ctx.MkSolver ()
    solver.Set ("timeout", 10000u)
    solver.Assert (conds)
    solver

  let isSAT (solver: Z3.Solver) (cond: Z3.BoolExpr) =
    match solver.Check (cond) with
    | Microsoft.Z3.Status.SATISFIABLE -> SAT
    | Microsoft.Z3.Status.UNSATISFIABLE -> UNSAT
    | _ -> TIMEOUT

  let getIdxSAT state (conds: Z3.BoolExpr []) cond =
    let state, cond = exprToZ3Cond state cond
    use solver = state.Ctx.MkSolver ()
    solver.Set ("timeout", 10000u)
    let length = Array.length conds
    let rec loop idx =
      if idx < length then
        solver.Assert (Array.get conds idx)
        match solver.Check (cond) with
        | Microsoft.Z3.Status.SATISFIABLE -> loop (idx + 1)
        | Microsoft.Z3.Status.UNSATISFIABLE -> idx
        | _ -> idx
      else Z3Exception "getIdxSAT fail" |> raise
    loop 0
