module LLIR.Builder

open System.Threading

open Common
open LLVM.Core

exception BuildExprException of string

let dummyVarID = -1, -1

let mkNum bv = Num bv

let mkZero size = BitVector.mkZero size |> mkNum

let True = mkNum BitVector.True
let False = mkNum BitVector.False
let ofBool c = if c then True else False

let unreachable = UnReachable
let nop = NOP

let ofInt v = BitVector.ofInt v |> mkNum

let ofInt64 v = BitVector.ofInt64 v |> mkNum

let mkInt v sz =
  if sz = 32 then ofInt v
  elif sz = 64 then int64 v |> ofInt64
  else sprintf "mkInt fail %d" sz |>  BuildExprException |> raise

let ofBytes size bytes = BitVector.ofBytes size bytes |> mkNum

let one32 = ofInt 1
let ptrSizeInt = 64
let ptrSize = ofInt ptrSizeInt
let nullPtr = mkZero ptrSizeInt

let zero9 = mkZero 9
let zero22 = mkZero 22
let zero31 = mkZero 31
let zero32 = mkZero 32
let zero51 = mkZero 51
let zero62 = mkZero 62
let zero64 = mkZero 64
let zero111 = mkZero 111

let mkMax size = BitVector.mkMax size |> mkNum
let max6 = mkMax 6
let max9 = mkMax 9
let max12 = mkMax 12
let max16 = mkMax 16
let max17 = mkMax 17

let getDepth = function
  | Arg _
  | Any _
  | BlockAddr _
  | LocalAddr _
  | Num _
  | GlobalVar _
  | Undef _
  | Var _
  | SymVar _
  | Metadata _ -> 0
  | UnIntCall (_, _, _, info)
  | Addr (_, _, info)
  | Load (_, _, info)
  | BinOp (_, _, _, _, info)
  | Cast (_, _, _, info)
  | Concat (_, _, info)
  | Extract (_, _, _, info)
  | RelOp (_, _, _, info)
  | Replace (_, _, _, _, info)
  | Select (_, _, _, _, info)
  | UnOp (_, _, _, info) -> info.Depth

let getInfo expr = { Depth = getDepth expr }

let getSize = function
  | Addr _ -> ptrSizeInt
  | Arg (_, size)
  | Any (_, size)
  | BinOp (_, _, _, size, _)
  | Cast (_, _, size, _)
  | Concat (_, size, _)
  | Load (_, size, _)
  | Select (_, _, _, size, _)
  | Var (_, size)
  | SymVar (_, size)
  | GlobalVar (_, size)
  | Extract (_, _, size, _)
  | UnOp (_, _, size, _)
  | UnIntCall (_, _, size, _)
  | Replace (_, _, _, size, _)
  | Undef (_, size) -> size
  | LocalAddr _
  | BlockAddr _ -> ptrSizeInt
  | Num bv -> BitVector.getLength bv
  | RelOp _ -> 1
  | Metadata _ -> 0

let inline isSameSize e1 e2 = getSize e1 = getSize e2

let inline isSizeN e n = getSize e = n

let inline isBE e n = getSize e >= n

let mergeInfo e1 e2 = { Depth = 0 } //{ Depth = (getDepth e1) + (getDepth e2) + 1 }

let mergeInfos exprs = { Depth = 0 }//{ Depth = (Array.map getDepth exprs |> Array.sum) + 1 }

let mutable private undefCnt = 0
let private undefLock = new Mutex ()

let mkUndef size =
  undefLock.WaitOne () |> ignore
  let undef = Undef (undefCnt, size)
  undefCnt <- undefCnt + 1
  undefLock.ReleaseMutex()
  undef

let mkArg idx size = Arg (idx, size)

let mkVar id size = Var (id, size)

let private revRelOp = function
  | EQ -> NE
  | NE -> EQ
  | ULT -> UGE
  | ULE -> UGT
  | UGT -> ULE
  | UGE -> ULT
  | SLT -> SGE
  | SLE -> SGT
  | SGT -> SLE
  | SGE -> SLT

let mkNeg expr size =
  match expr with
  | Num bv -> BitVector.neg bv |> mkNum
  | _ -> UnOp (NEG, expr, size, getInfo expr)

let mkNot expr =
  match expr with
  | Num bv -> BitVector.doNot bv |> mkNum
  | RelOp (op, e1, e2, info) -> RelOp (revRelOp op, e1, e2, info)
  | _ -> UnOp (NOT, expr, 1, getInfo expr)

let mkUnOp op expr size =
  match op with
  | NEG -> mkNeg expr size
  | NOT -> mkNot expr

let private buildBinOp ty e1 e2 (size: ExprSize) =
  BinOp (ty, e1, e2, size, mergeInfo e1 e2)

let mkAdd e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.add bv1 bv2 |> mkNum
  | Num bv, o
  | o, Num bv when BitVector.isZero bv -> o
  | BinOp (ADD, e, Num bv1, _, _), Num bv2
  | BinOp (ADD, Num bv1, e, _, _), Num bv2
  | Num bv2, BinOp (ADD, e, Num bv1, _, _)
  | Num bv2, BinOp (ADD, Num bv1, e, _, _) ->
    buildBinOp ADD e (BitVector.add bv1 bv2 |> mkNum) size
  | _, _ -> buildBinOp ADD e1 e2 size

let rec mkSub e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.sub bv1 bv2 |> mkNum
  | o, Num bv when BitVector.isZero bv -> o
  | Num bv, o when BitVector.isZero bv -> mkNeg o size
  | BinOp (ADD, e, Num bv1, _, _), Num bv2
  | BinOp (ADD, Num bv1, e, _, _), Num bv2 ->
    mkAdd e (BitVector.sub bv1 bv2 |> mkNum) size
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkSub p1 p2 size
  | _, _ -> buildBinOp SUB e1 e2 size

let rec mkAddr base_ idx =
  match base_ with
  | Num _ -> mkAdd base_ idx (getSize base_)
  | BinOp (ADD, e, Num bv, _, _)
  | BinOp (ADD, Num bv, e, _, _) ->
    mkAddr e (mkAdd (mkNum bv) idx (getSize idx))
  | Addr (base_, idx_, _) ->
    Addr (base_, mkAdd idx_ idx (getSize idx), mergeInfo base_ idx)
  | _ -> Addr (base_, idx, mergeInfo base_ idx)

let rec mkMul e1 e2 sz =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.mul bv1 bv2 |> mkNum
  | Num bv, o
  | o, Num bv when BitVector.isZero bv -> mkNum bv
  | Num bv, o
  | o, Num bv when BitVector.isOne bv -> o
  | BinOp (UDIV, e, v1, _, _), v2
  | v2, BinOp (UDIV, e, v1, _, _) when v1 = v2 -> e
  | BinOp (ADD, e1, e2, _, _), e3
  | e3, BinOp (ADD, e1, e2, _, _) -> mkAdd (mkMul e1 e3 sz) (mkMul e2 e3 sz) sz
  | BinOp (SUB, e1, e2, _, _), e3
  | e3, BinOp (SUB, e1, e2, _, _) -> mkSub (mkMul e1 e3 sz) (mkMul e2 e3 sz) sz
  | Addr (e1, e2, _), e3 -> mkAddr (mkMul e1 e3 sz) (mkMul e2 e3 sz)
  | _, _ -> buildBinOp MUL e1 e2 sz

let mkMulInt e1 v =
  let size = getSize e1
  mkMul e1 (BitVector.ofIntLen size v |> mkNum) size

let mkMulInt_ v e1 = mkMulInt e1 v

let rec mkUDiv e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.udiv bv1 bv2 |> mkNum
  | o, Num bv when BitVector.isOne bv -> o
  | BinOp (MUL, e, v1, _, _), v2 when v1 = v2 -> e
  | Addr (e1, e2, _), e3 -> mkAddr (mkUDiv e1 e3 size) (mkUDiv e2 e3 size)
  | _, _ -> buildBinOp UDIV e1 e2 size

let mkUDivInt e v =
  let size = getSize e
  mkUDiv e (BitVector.ofIntLen size v |> mkNum) size

let mkUDivInt_ v e = mkUDivInt e v

let mkSDiv e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.sdiv bv1 bv2 |> mkNum
  | o, Num bv when BitVector.isOne bv -> o
  | _, _ -> buildBinOp SDIV e1 e2 size

let mkURem e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.urem bv1 bv2 |> mkNum
  | o, Num bv when BitVector.isOne bv -> mkZero size
  | _, _ -> buildBinOp SDIV e1 e2 size

let mkSRem e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.srem bv1 bv2 |> mkNum
  | o, Num bv when BitVector.isOne bv -> mkZero size
  | _, _ -> buildBinOp SDIV e1 e2 size

let mkAnd e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.doAnd bv1 bv2 |> mkNum
  | Num bv, e
  | e, Num bv when BitVector.isZero bv -> mkNum bv
  | Num bv, e
  | e, Num bv when BitVector.isAllTrue bv -> e
  | _, _ -> buildBinOp AND e1 e2 size

let mkOr e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.doOr bv1 bv2 |> mkNum
  | Num bv, e
  | e, Num bv when BitVector.isZero bv -> e
  | Num bv, e
  | e, Num bv when BitVector.isAllTrue bv -> mkNum bv
  | _, _ -> buildBinOp OR e1 e2 size

let mkXor e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.xor bv1 bv2 |> mkNum
  | _, _ -> buildBinOp XOR e1 e2 size

let mkShl e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.shl bv1 bv2 |> mkNum
  | _, _ -> buildBinOp SHL e1 e2 size

let mkLShr e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.lShr bv1 bv2 |> mkNum
  | _, _ -> buildBinOp LSHR e1 e2 size

let mkAShr e1 e2 size =
  match e1, e2 with
  | Num bv1, Num bv2 -> BitVector.aShr bv1 bv2 |> mkNum
  | _, _ -> buildBinOp ASHR e1 e2 size

let mkUnIntCall callee args size =
  UnIntCall (callee, args, size, mergeInfos args)

let mkBinOp op e1 e2 size =
  match op with
  | ADD  -> mkAdd e1 e2 size
  | SUB  -> mkSub e1 e2 size
  | MUL  -> mkMul e1 e2 size
  | UDIV -> mkUDiv e1 e2 size
  | SDIV -> mkSDiv e1 e2 size
  | UREM -> mkURem e1 e2 size
  | SREM -> mkSRem e1 e2 size
  | AND -> mkAnd e1 e2 size
  | OR -> mkOr e1 e2 size
  | XOR -> mkXor e1 e2 size
  | SHL -> mkShl e1 e2 size
  | LSHR -> mkLShr e1 e2 size
  | ASHR -> mkAShr e1 e2 size

let private buildRelOp ty e1 e2 = RelOp (ty, e1, e2, mergeInfo e1 e2)

let rec mkEq e1 e2 =
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkEq p1 p2
  | Num bv1, Num bv2 -> if bv1 = bv2 then True else False
  | e1, e2 when e1 = e2 -> True
  | _, _ -> buildRelOp EQ e1 e2

let rec mkNe e1 e2 =
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkNe p1 p2
  | Num bv1, Num bv2 -> if bv1 = bv2 then False else True
  | e1, e2 when e1 = e2 -> False
  | _, _ -> buildRelOp NE e1 e2

let rec mkUlt e1 e2 =
  (* e1 |<| e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkUlt p1 p2
  | Num bv1, Num bv2 -> BitVector.ult bv1 bv2 |> mkNum
  | _, Num bv when BitVector.isZero bv -> False
  | Num bv, _ when BitVector.isAllTrue bv -> False
  | Cast (ZEXT, e, sz, _), Num bv when
    BitVector.ult (BitVector.zext (BitVector.mkMax (getSize e)) sz) bv =
    BitVector.True ->
    True
  | _, _ -> buildRelOp ULT e1 e2

let rec mkUle e1 e2 =
  (* e1 |<=| e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkUle p1 p2
  | Num bv1, Num bv2 -> BitVector.ule bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isZero bv -> True
  | _, Num bv when BitVector.isAllTrue bv -> True
  | Cast (ZEXT, e, sz, _), Num bv when
    BitVector.ule (BitVector.zext (BitVector.mkMax (getSize e)) sz) bv =
    BitVector.True ->
    True
  | _, _ -> buildRelOp ULE e1 e2

let rec mkUgt e1 e2 =
  (* e1 |>| e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkUgt p1 p2
  | Num bv1, Num bv2 -> BitVector.ugt bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isZero bv -> False
  | _, Num bv when BitVector.isAllTrue bv -> False
  | Num bv, Cast (ZEXT, e, sz, _) when
    BitVector.ult (BitVector.zext (BitVector.mkMax (getSize e)) sz) bv =
    BitVector.True ->
    True
  | _, _ -> buildRelOp UGT e1 e2

let rec mkUge e1 e2 =
  (* e1 |>=| e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkUge p1 p2
  | Num bv1, Num bv2 -> BitVector.uge bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isAllTrue bv -> True
  | _, Num bv when BitVector.isZero bv -> True
  | Num bv, Cast (ZEXT, e, sz, _) when
    BitVector.ule (BitVector.zext (BitVector.mkMax (getSize e)) sz) bv =
    BitVector.True ->
    True
  | _, _ -> buildRelOp UGE e1 e2

let rec mkSlt e1 e2 =
  (* e1 < e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkSlt p1 p2
  | Num bv1, Num bv2 -> BitVector.slt bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isSMax bv -> False
  | _, Num bv when BitVector.isSMin bv -> False
  | _, _ -> buildRelOp SLT e1 e2

let rec mkSle e1 e2 =
  (* e1 <= e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkSle p1 p2
  | Num bv1, Num bv2 -> BitVector.sle bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isSMin bv -> True
  | _, Num bv when BitVector.isSMax bv -> True
  | _, _ -> buildRelOp SLE e1 e2

let rec mkSgt e1 e2 =
  (* e1 > e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkSgt p1 p2
  | Num bv1, Num bv2 -> BitVector.sgt bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isSMax bv -> False
  | _, Num bv when BitVector.isSMin bv -> False
  | _,  _ -> buildRelOp SGT e1 e2

let rec mkSge e1 e2 =
  (* e1 >= e2 *)
  match e1, e2 with
  | Addr (e1, p1, _), Addr (e2, p2, _)
  | Addr (p1, e1, _), Addr (p2, e2, _) when e1 = e2 -> mkSge p1 p2
  | Num bv1, Num bv2 -> BitVector.sge bv1 bv2 |> mkNum
  | Num bv, _ when BitVector.isSMax bv -> True
  | _, Num bv when BitVector.isSMin bv -> True
  | _,  _ -> buildRelOp SGE e1 e2

let mkRelOp ty e1 e2 =
  match ty with
  | EQ -> mkEq e1 e2
  | NE -> mkNe e1 e2
  | ULT -> mkUlt e1 e2
  | ULE -> mkUle e1 e2
  | UGT -> mkUgt e1 e2
  | UGE -> mkUge e1 e2
  | SLT -> mkSlt e1 e2
  | SLE -> mkSle e1 e2
  | SGT -> mkSgt e1 e2
  | SGE -> mkSge e1 e2

let mkSelect cond tExpr fExpr size =
  if cond = True then tExpr
  elif cond = False then fExpr
  else Select (cond, tExpr, fExpr, size, mergeInfos [|cond; tExpr; fExpr|])

let mkZEXT expr size =
  if isSizeN expr size then expr
  else
    match expr with
    | Num bv -> BitVector.zext bv size |> mkNum
    | _ -> Cast (ZEXT, expr, size, getInfo expr)

let mkSEXT expr size =
  if isSizeN expr size then expr
  else
    match expr with
    | Num bv -> BitVector.sext bv size |> mkNum
    | _ -> Cast (SEXT, expr, size, getInfo expr)

let rec mkExtract src pos size =
  match src, pos with
  | Num src, Num pos -> BitVector.extract src pos size |> mkNum
  | src, Num bv when BitVector.isZero bv && isSizeN src size -> src
  | Concat (exprs, _, _), Num bv ->
    match mkExtractOfConcat exprs (BitVector.toInt bv) size with
    | Some ret -> ret
    | _ -> Extract (src, pos, size, mergeInfo src pos)
  | Replace (ddst, Num dpos, dsrc, _, _), Num bv
    when BitVector.toInt dpos <= BitVector.toInt bv &&
      (BitVector.toInt dpos) + (getSize dsrc) >= (BitVector.toInt bv) + size ->
    let off = (BitVector.toInt bv) - (BitVector.toInt dpos)
    mkExtract dsrc (ofInt off) size
  | Replace (ddst, Num dpos, _, _, _), Num bv
    when (BitVector.toInt bv) + size <= BitVector.toInt dpos ->
    mkExtract ddst pos size
  | Cast (_, expr, _, _), Num p when BitVector.toInt p + size <= getSize expr ->
    mkExtract expr pos size
  | _, _ -> Extract (src, pos, size, mergeInfo src pos)

and mkExtractOfConcat exprs pos size =
  let rec loop idx = function
    | expr :: remain ->
      let eSize = getSize expr
      if idx <= pos && pos + size <= idx + eSize then
        mkExtract expr (pos - idx |> ofInt) size |> Some
      elif pos > idx then loop (idx + eSize) remain
      else None
    | _ -> None
  loop 0 (Array.toList exprs)

let mkCast ty expr size =
  match ty with
  | ZEXT -> mkZEXT expr size
  | SEXT -> mkSEXT expr size

let private sumSize exprs = Array.map getSize exprs |> Array.sum

let private mkConcatFolder ret expr =
  match ret, expr with
  | [], _ -> [expr]
  | Num bv1 :: remain, Num bv2 -> mkNum (BitVector.concat bv1 bv2) :: remain
  | _, _ -> expr :: ret

let mkConcat exprs size =
  if sumSize exprs = size then
    match Array.fold mkConcatFolder [] exprs with
    | [] -> BuildExprException "mkConcat" |> raise
    | [expr] -> expr
    | exprs ->
      let exprs = Array.revList exprs
      let info = mergeInfos exprs
      Concat (exprs, size, info)
  else BuildExprException "mkConcat" |> raise

let mkLoad addr size = Load (addr, size, getInfo addr)

let mkLocalAddr idx = LocalAddr idx

let mkGlobalVar id size = GlobalVar (id, size)

let mkGlobal id (value:Expr) md isWritable : GlobalVar = {
  ID = id
  Value = value
  Metadata = md
  IsWritable = isWritable
}

let mkReplace dst offset src =
  match dst, offset, src with
  | _, Num off, _ when BitVector.toInt off = 0 && isSameSize dst src -> src
  | Num dst, Num offset, Num src ->
    BitVector.replace dst (BitVector.toInt offset) src |> mkNum
  | _, _, _ ->
    let info = mergeInfos [|dst; offset; src|]
    Replace (dst, offset, src, getSize dst, info)

let mkBlockAddr addr = BlockAddr addr

let mkAny exprs size = Any (exprs, size)

let mkSymVar name size = SymVar (name, size)

let mkMetadata ptr md = Metadata (ptr, md)

(* Statement *)

let mkJmp id = Jmp id

let mkIndJmp expr targets = IndJmp (expr, targets)

let mkCondJmp cond tID fID = CondJmp (cond, tID, fID)

let mkSwitch cond cases _default = Switch (cond, cases, _default)

let mkAlloca name size = Alloca (name, size)

let mkStore addr value = Store (addr, value)

let mkDef id expr = Def (id, expr)

let mkCalleeID id = Callee.ID id

let mkCalleeExpr expr = Callee.Expr expr

let mkInlineAsm code cons = Callee.InlineAsm (code, cons)

let private mergeCallInfos callee args =
  match callee with
  | Callee.Expr expr -> Array.append args [|expr|] |> mergeInfos
  | _ -> mergeInfos args

let mkCall name callee args retSize =
  Call (name, callee, args, retSize, mergeCallInfos callee args)

let mkTry name callee args retSize nID eID =
  Try (name, callee, args, retSize, nID, eID, mergeCallInfos callee args)

let mkThrow expr = Throw (expr, getInfo expr)

let mkCatch id size = Catch (id, size)

let mkPhi id map size =
  Phi (id, map, size, Map.toArray map |> Array.map snd |> mergeInfos)

let emptyFuncMD  = { RetVal = None; Args = [||] }
let mkFuncMD ret args = { RetVal = ret; Args = args }

let mkFunc id args body nodes rets edges isDeclr md = {
  ID = id
  Args = args
  Body = body
  Nodes = nodes
  RetNodes = rets
  Edges = edges
  IsDeclr = isDeclr
  Metadata = md
  Calls = Map.empty
}
