namespace LLIR

open System
open System.Collections
open LLVM
open LLVM.Core
open Common
open Common.Utils
open LLIR.Builder

exception LiftException of string

type LiftContext = {
  Funcs: Map<Name, Function>
  Metadatas: LLVM.Metadatas
  BlockMap: Map<Name, BlockID>
  VarMap: Map<Name, VarID>
  ConstTCache: Concurrent.ConcurrentDictionary<LLVMPtr, Expr>
  MetadataCache: Concurrent.ConcurrentDictionary<MetadataRef, Metadata>
}

module LiftContext =
  let private initFolder ret (name, _) func = Map.add name func ret

  let init funcs metadatas = {
    Funcs = Map.fold initFolder Map.empty funcs
    Metadatas = metadatas
    BlockMap = Map.empty
    VarMap = Map.empty
    ConstTCache = new Concurrent.ConcurrentDictionary<LLVMPtr, Expr> ()
    MetadataCache = new Concurrent.ConcurrentDictionary<MetadataRef, Metadata>()
  }

  let private getLiftedSize = function
    | CmpXchg _
    | AtomicRMW _ -> 2
    | _ -> 1

  let private mkBlockMap blockID (ret, idx) (id, instr) =
    Map.add id (blockID, idx) ret, idx + (getLiftedSize instr)

  let private getVarMap blockID (_, instrs) =
    Array.fold (mkBlockMap blockID) (Map.empty, 0) instrs |> fst

  let private addArgs args map =
    let folder map idx (_, name) = Map.add name (-1, idx) map
    Array.foldi folder map args

  let addBlocks ctx blocks args = {
    ctx with VarMap = Array.mapi getVarMap blocks |> Map.union |> addArgs args
             BlockMap = Array.mapi (fun idx (id, _) -> id, idx) blocks
                        |> Map.ofArray
  }

  let getVarID ctx name = if name = "" then -1, -1 else Map.find name ctx.VarMap

  let getBlockID ctx name = Map.find name ctx.BlockMap

  let resolveBlockID ctx fID bID =
    let func = Map.find fID ctx.Funcs
    fID, Array.findIndex (fun (name, _) -> name = bID) (Function.getBB func)

  let getConstTCache ctx = ctx.ConstTCache

  let addMetadata ctx ptr md = ctx.MetadataCache.TryAdd (ptr, md) |> ignore

  let getMetadatas ctx = ConcurrentDictionary.toMap ctx.MetadataCache

  let getMDCache ctx = ctx.MetadataCache

  let getMD ctx ptr = Map.find ptr ctx.Metadatas
  let getMD2 ctx ptr = ptr, getMD ctx ptr

module Lift =
  let liftLogicOpType = function
    | Shl  -> SHL
    | LShr -> LSHR
    | AShr -> ASHR
    | LogicOpType.And  -> AND
    | LogicOpType.Or   -> OR
    | LogicOpType.Xor  -> XOR

  let liftICmpOpType = function
    | ICmpType.Eq  -> EQ
    | ICmpType.Ne  -> NE
    | ICmpType.Ugt -> UGT
    | ICmpType.Uge -> UGE
    | ICmpType.Ult -> ULT
    | ICmpType.Ule -> ULE
    | ICmpType.Sgt -> SGT
    | ICmpType.Sge -> SGE
    | ICmpType.Slt -> SLT
    | ICmpType.Sle -> SLE

  let liftUnOpType = function
    | FNeg -> NEG

  let private resolveNamedTy = function
    | Named (id, cons) -> AST.resolveNamedTy (id, cons)
    | ty -> ty

  let private exprToInt = function
    | Num bv -> BitVector.toInt bv
    | _ -> LiftException "exprToInt" |> raise

  let private calcOffset items n =
    let ty, offset = Array.get items n
    offset, resolveNamedTy ty

  let private liftGetElemPtrFolder (addr, ty) indice =
    match ty with
    | Pointer (ty, _, _)
    | Type.Array (ty, _, _)
    | Type.Vector (ty, _, _) ->
      let ty = resolveNamedTy ty
      let indice = mkZEXT indice ptrSizeInt
      let size = Type.getSize ty |> int64 |> ofInt64
      mkAdd addr (mkMul size indice ptrSizeInt) ptrSizeInt, ty
    | Type.Struct ((_, items, _, _, _), _) ->
      let offset, ty = calcOffset items (exprToInt indice)
      mkAdd addr (int64 offset |> ofInt64) ptrSizeInt, ty
    | _ -> LiftException "liftGetElemPtrFolder" |> raise

  let liftGetElemPtr ty addr indices =
    Array.fold liftGetElemPtrFolder (Builder.zero64, ty) indices |> fst
    |> mkAddr addr

  let mkUnIntFcmpCall func ty e1 e2 =
    let func = sprintf "%s%s" func (LLVM.Pp.floatTyToStr ty)
    mkUnIntCall func [|e1; e2|] 1

  let mkUnIntFcall func ty e1 e2 =
    let func = sprintf "%s%s" func (LLVM.Pp.floatTyToStr ty)
    mkUnIntCall func [|e1; e2|] (Type.getFloatSize ty)

  let liftFpToUI srcTy expr dstSize =
    match expr with
    | Num bv ->
      match BitVector.fpToUI bv srcTy dstSize with
      | Some bv -> mkNum bv
      | _ -> mkUndef dstSize
    | _ ->
      let func = sprintf "%sToUI%d" (LLVM.Pp.floatTyToStr srcTy) dstSize
      mkUnIntCall func [|expr|] dstSize

  let liftFpToSI srcTy expr dstSize =
    match expr with
    | Num bv ->
      match BitVector.fpToSI bv srcTy dstSize with
      | Some bv -> mkNum bv
      | _ -> mkUndef dstSize
    | _ ->
      let func = sprintf "%sToSI%d" (LLVM.Pp.floatTyToStr srcTy) dstSize
      mkUnIntCall func [|expr|] dstSize

  let liftUiToFp srcSize dstTy expr _ =
    match expr with
    | Num bv -> BitVector.uiToFp bv srcSize dstTy
    | _ ->
      let func = sprintf "UI%dTo%s" srcSize (LLVM.Pp.floatTyToStr dstTy)
      mkUnIntCall func [|expr|] (Type.getFloatSize dstTy)

  let liftSiToFp srcSize dstTy expr _ =
    match expr with
    | Num bv -> BitVector.siToFp bv srcSize dstTy
    | _ ->
      let func = sprintf "SI%dTo%s" srcSize (LLVM.Pp.floatTyToStr dstTy)
      mkUnIntCall func [|expr|] (Type.getFloatSize dstTy)

  let private convFp expr srcTy dstTy =
    match expr with
    | Num bv -> BitVector.convFp bv srcTy dstTy |> mkNum
    | expr ->
      if srcTy = dstTy then expr
      else
        let size = Type.getFloatSize dstTy
        let srcTy = LLVM.Pp.floatTyToStr srcTy
        let dstTy = LLVM.Pp.floatTyToStr dstTy
        let func = sprintf "%sTo%s" srcTy dstTy
        mkUnIntCall func [|expr|] size

  let private liftCastVector lift srcSize dstSize n e =
    let rec loop ret i =
      if i < n then
        let pos = ofInt (srcSize * i)
        let e = mkExtract e pos srcSize
        loop (lift e dstSize :: ret) (i + 1)
      else ret
    mkConcat (Array.revList (loop [] 0)) (dstSize * n)

  let liftFpTrunc expr srcTy dstTy =
    let srcTy = Type.toFloatTy srcTy
    let dstTy = Type.toFloatTy dstTy
    if Type.getFloatSize srcTy < Type.getFloatSize dstTy then
      BuildExprException "liftFpTrunc" |> raise
    else convFp expr srcTy dstTy

  let liftFpExtOne srcSize srcTy dstTy expr dstSize =
    if srcSize > dstSize then BuildExprException "liftFpExt" |> raise
    else convFp expr srcTy dstTy

  let liftFpExt expr srcTy dstTy =
    match srcTy, dstTy with
    | Type.Float (srcTy, _), Type.Float (dstTy, _) ->
      Type.getFloatSize dstTy
      |> liftFpExtOne (Type.getFloatSize srcTy) srcTy dstTy expr
    | Type.Vector (Type.Float (srcTy, _), n, _),
      Type.Vector (Type.Float (dstTy, _), m, _) ->
      if n = m then
        let srcSize = Type.getFloatSize srcTy
        let dstSize = Type.getFloatSize dstTy
        liftCastVector (liftFpExtOne srcSize srcTy dstTy) srcSize dstSize n expr
      else BuildExprException "liftFpExt" |> raise
    | _ -> BuildExprException "liftFpExt" |> raise

  let private liftIntCast lift expr srcTy dstTy =
    match srcTy, dstTy with
    | Integer _, Integer (dstSize, _) -> lift expr dstSize
    | Type.Vector (Integer (srcSize, _), n, _),
      Type.Vector (Integer (dstSize, _), m, _) ->
      if n = m then liftCastVector lift srcSize dstSize n expr
      else BuildExprException "liftIntCast" |> raise
    | _ -> BuildExprException "liftIntCast" |> raise

  let private liftFpToI lift expr srcTy dstTy =
    match srcTy, dstTy with
    | Type.Float (srcTy, _), Integer (dstSize, _) -> lift srcTy expr dstSize
    | Type.Vector (Type.Float (srcTy, _), n, _),
      Type.Vector (Integer (dstSize, _), m, _) ->
      if n = m then
        liftCastVector (lift srcTy) (Type.getFloatSize srcTy) dstSize n expr
      else BuildExprException "liftFpToI" |> raise
    | _ -> BuildExprException "liftFpToI" |> raise

  let private liftIntToFp lift expr srcTy dstTy =
    match srcTy, dstTy with
    | Integer (srcSize, _), Type.Float (dstTy, _) ->
      lift srcSize dstTy expr (Type.getFloatSize dstTy)
    | Type.Vector (Integer (srcSize, _), n, _),
      Type.Vector (Type.Float (dstTy, _), m, _) ->
      if n = m then
        let dstSize = Type.getFloatSize dstTy
        liftCastVector (lift srcSize dstTy) srcSize dstSize n expr
      else BuildExprException "liftIntToFp" |> raise
    | _ -> BuildExprException "liftIntToFp" |> raise

  let private liftTruncOne expr dstSize = mkExtract expr zero32 dstSize

  let private truncOrZExt srcSize expr dstSize =
    if srcSize = dstSize then expr
    elif srcSize < dstSize then mkZEXT expr dstSize
    else mkExtract expr zero32 dstSize

  let private liftPtrToInt expr srcTy dstTy =
    match srcTy, dstTy with
    | Pointer _, Integer (dstSize, _) -> truncOrZExt ptrSizeInt expr dstSize
    | Type.Vector (Pointer _, n, _),
      Type.Vector (Integer (dstSize, _), m, _) ->
      if n = m then
        liftCastVector (truncOrZExt ptrSizeInt) ptrSizeInt dstSize n expr
      else BuildExprException "liftPtrToInt" |> raise
    | _ -> BuildExprException "liftPtrToInt" |> raise
    |> Builder.mkUDivInt_ 8

  let private liftIntToPtr expr srcTy dstTy =
    match srcTy, dstTy with
    | Integer (srcSize, _), Pointer _ -> truncOrZExt srcSize expr ptrSizeInt
    | Type.Vector (Integer (srcSize, _), n, _),
      Type.Vector (Pointer _, m , _) ->
      if n = m then
        liftCastVector (truncOrZExt srcSize) srcSize ptrSizeInt n expr
      else BuildExprException "liftIntToPtr" |> raise
    | _ -> BuildExprException "liftIntToPtr" |> raise
    |> Builder.mkMulInt_ 8

  let liftCast op expr srcTy dstTy =
    match op with
    | Trunc -> liftIntCast liftTruncOne expr srcTy dstTy
    | ZExt -> liftIntCast mkZEXT expr srcTy dstTy
    | SExt -> liftIntCast mkSEXT expr srcTy dstTy
    | FPToUI -> liftFpToI liftFpToUI expr srcTy dstTy
    | FPToSI -> liftFpToI liftFpToSI expr srcTy dstTy
    | UIToFP -> liftIntToFp liftUiToFp expr srcTy dstTy
    | SIToFP -> liftIntToFp liftSiToFp expr srcTy dstTy
    | FPTrunc -> liftFpTrunc expr srcTy dstTy
    | FPExt -> liftFpExt expr srcTy dstTy
    | PtrToInt -> liftPtrToInt expr srcTy dstTy
    | IntToPtr -> liftIntToPtr expr srcTy dstTy
    | BitCast -> expr
    | AddrSpaceCast -> failwith "Not Supported yet"

  let liftExtractElem ty vec idx =
    match ty with
    | Type.Vector (elemTy, _, _) ->
      let size = Type.getSize elemTy
      let pos = mkMulInt idx size
      mkExtract vec pos size
    | _ -> LiftException "liftExtractElem" |>  raise

  let liftInsertElem ty dst idx src =
    match ty with
    | Type.Vector (_, _, _) -> mkReplace dst idx src
    | _ -> LiftException "liftInsertElem" |>  raise

  let private maskToInt (_, cons, _) =
    match cons with
    | Int (32, bytes) -> BitConverter.ToInt32 (bytes, 0)
    | Undef _ -> -1
    | _ -> LiftException "maskToInt" |>  raise

  let private maskToInts = function
    | AggregateZero (Type.Vector (_, length, _)) -> Array.create length 0
    | Constant.Vector vector -> Array.map maskToInt vector
    | _ -> LiftException "maskToInts" |> raise

  let private isCont mask start n =
    if Array.length mask = n then
      Array.init n (fun x -> x + start) = mask
    else false

  let liftShuffle ty v1 v2 (_, mask, _) =
    let size, num = Type.getVectorSize ty
    let mask = maskToInts mask
    if isCont mask 0 num then v1
    elif isCont mask num num then v2
    else
      let length = Array.length mask
      let getter n =
        if n < 0 then mkUndef size
        elif n < num then mkExtract v1 (ofInt (n * size)) size
        else mkExtract v2 (ofInt ((n - num) * size)) size
      mkConcat (Array.map getter mask) (size * length)

  let private calcIndiceFolder (addr, ty) indice =
    match ty with
    | Pointer (ty, _, _)
    | Type.Array (ty, _, _)
    | Type.Vector (ty, _, _) ->
      let ty = resolveNamedTy ty
      (Type.getSize ty) + indice, ty
    | Type.Struct ((_, items, _, _, _), _) ->
      let offset, ty = calcOffset items indice
      addr + offset, ty
    | _ -> LiftException "liftGetElemPtr" |> raise

  let private calcIndice ty indices =
    Array.fold calcIndiceFolder (0, ty) indices

  let liftExtractVal ty expr indices =
    let offset, ty = calcIndice ty indices
    mkExtract expr (ofInt offset) (Type.getSize ty)

  let liftInsertVal ty dst indices src =
    let offset = calcIndice ty indices |> fst |> ofInt
    mkReplace dst offset src

  let inline checkQNaN e frac expo fracLen expoLen =
    let c1 = mkNe frac (mkExtract e zero32 fracLen)
    let c2 = mkEq expo (mkExtract e (ofInt fracLen) expoLen)
    mkAnd c1 c2 1

  let isQNaN ty e =
    match ty with
    | LLVM.FloatTy.Half -> checkQNaN e zero9 max6 9 6
    | LLVM.FloatTy.Float -> checkQNaN e zero22 max9 22 9
    | LLVM.FloatTy.Double -> checkQNaN e zero51 max12 51 12
    | LLVM.FloatTy.Fp128 -> checkQNaN e zero111 max16 111 16
    | LLVM.FloatTy.X86Fp80 -> checkQNaN e zero62 max17 62 17
    | LLVM.FloatTy.PpcFp128 -> failwith "Not Supported yet"

  let notQNaN ty e = isQNaN ty e |> mkNot
  let notQNaNs ty e1 e2 = mkAnd (notQNaN ty e1) (notQNaN ty e2) 1
  let anyQNaNs ty e1 e2 = mkOr (isQNaN ty e1) (isQNaN ty e2) 1

  let fcmpGT ty e1 e2 =
    match e1, e2 with
    | Num bv1, Num bv2 -> BitVector.fcmpGT ty bv1 bv2 |> ofBool
    | _, _ -> mkUnIntFcmpCall "fcmpGT" ty e1 e2

  let fcmpGE ty e1 e2 = fcmpGT ty e2 e1 |> mkNot
  let fcmpLT ty e1 e2 = fcmpGT ty e2 e1
  let fcmpLE ty e1 e2 = fcmpGE ty e2 e1

  let liftFCmpOne op ty e1 e2 =
    match op with
    | False -> False
    | Oeq -> mkAnd (notQNaNs ty e1 e2) (mkEq e1 e2) 1
    | Ogt -> mkAnd (notQNaNs ty e1 e2) (fcmpGT ty e1 e2) 1
    | Oge -> mkAnd (notQNaNs ty e1 e2) (fcmpGE ty e1 e2) 1
    | Olt -> mkAnd (notQNaNs ty e1 e2) (fcmpLT ty e1 e2) 1
    | Ole -> mkAnd (notQNaNs ty e1 e2) (fcmpLE ty e1 e2) 1
    | One -> mkAnd (notQNaNs ty e1 e2) (mkNe e1 e2) 1
    | Ord -> notQNaNs ty e1 e2
    | Uno -> anyQNaNs ty e1 e2
    | Ueq -> mkOr (anyQNaNs ty e1 e2) (mkEq e1 e2) 1
    | Ugt -> mkOr (anyQNaNs ty e1 e2) (fcmpGT ty e1 e2) 1
    | Uge -> mkOr (anyQNaNs ty e1 e2) (fcmpGE ty e1 e2) 1
    | Ult -> mkOr (anyQNaNs ty e1 e2) (fcmpLT ty e1 e2) 1
    | Ule -> mkOr (anyQNaNs ty e1 e2) (fcmpLE ty e1 e2) 1
    | Une -> mkOr (anyQNaNs ty e1 e2) (mkNe e1 e2) 1
    | True -> True

  let liftFAdd ty e1 e2 =
    match ty, e1, e2 with
    | _, Num bv1, Num bv2 -> BitVector.fadd ty bv1 bv2 |> mkNum
    | _, _, _ -> mkUnIntFcall "fadd" ty e1 e2

  let liftFSub ty e1 e2 =
    match ty, e1, e2 with
    | _, Num bv1, Num bv2 -> BitVector.fsub ty bv1 bv2 |> mkNum
    | _, _, _ -> mkUnIntFcall "fsub" ty e1 e2

  let liftFMul ty e1 e2 =
    match ty, e1, e2 with
    | _, Num bv1, Num bv2 -> BitVector.fmul ty bv1 bv2 |> mkNum
    | _, _, _ -> mkUnIntFcall "fmul" ty e1 e2

  let liftFDiv ty e1 e2 =
    match ty, e1, e2 with
    | _, Num bv1, Num bv2 -> BitVector.fdiv ty bv1 bv2 |> mkNum
    | _, _, _ -> mkUnIntFcall "fdiv" ty e1 e2

  let liftFRem ty e1 e2 =
    match ty, e1, e2 with
    | _, Num bv1, Num bv2 -> BitVector.frem ty bv1 bv2 |> mkNum
    | _, _, _ -> mkUnIntFcall "frem" ty e1 e2

  let liftBinaryOne op ty e1 e2 =
    match op with
    | BinOpType.Add  -> mkAdd e1 e2 (Type.getSize ty)
    | BinOpType.Sub  -> mkSub e1 e2 (Type.getSize ty)
    | BinOpType.Mul  -> mkMul e1 e2 (Type.getSize ty)
    | BinOpType.UDiv -> mkUDiv e1 e2 (Type.getSize ty)
    | BinOpType.SDiv -> mkSDiv e1 e2 (Type.getSize ty)
    | BinOpType.URem -> mkURem e1 e2 (Type.getSize ty)
    | BinOpType.SRem -> mkSRem e1 e2 (Type.getSize ty)
    | BinOpType.FAdd -> liftFAdd (Type.toFloatTy ty) e1 e2
    | BinOpType.FSub -> liftFSub (Type.toFloatTy ty) e1 e2
    | BinOpType.FMul -> liftFMul (Type.toFloatTy ty) e1 e2
    | BinOpType.FDiv -> liftFDiv (Type.toFloatTy ty) e1 e2
    | BinOpType.FRem -> liftFRem (Type.toFloatTy ty) e1 e2

  let private liftOpVector lift retSize size n e1 e2 =
    let rec loop ret i =
      if i < n then
        let pos = ofInt (size * i)
        let e1 = mkExtract e1 pos size
        let e2 = mkExtract e2 pos size
        loop (lift e1 e2 :: ret) (i + 1)
      else ret
    mkConcat (Array.revList (loop [] 0)) retSize

  let liftBinary op ty e1 e2 =
    match ty with
    | Integer _
    | Type.Float _ -> liftBinaryOne op ty e1 e2
    | Type.Vector (ty, n, _) ->
      match ty with
      | Integer (size, _) ->
        liftOpVector (liftBinaryOne op ty) (size * n) size n e1 e2
      | Type.Float (floatTy, _) ->
        let size = Type.getFloatSize floatTy
        liftOpVector (liftBinaryOne op ty) (size * n) size n e1 e2
      | _ -> LiftException "liftBinary" |>  raise
    | _ -> LiftException "liftBinary" |>  raise


  let liftLogic op ty e1 e2 =
    let op = liftLogicOpType op
    let mkBinOp_ size e1 e2 = mkBinOp op e1 e2 size
    match ty with
    | Integer (size, _) -> mkBinOp op e1 e2 size
    | Type.Vector (Integer (size, _), n, _) ->
      liftOpVector (mkBinOp_ size) (size * n) size n e1 e2
    | _ -> LiftException "liftLogic" |>  raise

  let liftICmp op ty e1 e2 =
    let op = liftICmpOpType op
    match ty with
    | Integer _
    | Pointer _ -> mkRelOp op e1 e2
    | Type.Vector (Integer (size, _), n, _) ->
      liftOpVector (mkRelOp op) n size n e1 e2
    | Type.Vector (Pointer _, n, _) ->
      liftOpVector (mkRelOp op) n ptrSizeInt n e1 e2
    | _ -> LiftException "liftICmp" |>  raise

  let liftFCmp op ty e1 e2 =
    match ty with
    | Type.Float (ty, _) -> liftFCmpOne op ty e1 e2
    | Type.Vector (Type.Float (ty, _), n, _) ->
      liftOpVector (liftFCmpOne op ty) n (Type.getFloatSize ty) n e1 e2
    | _ -> LiftException "liftFCmp" |>  raise

  let private calcFieldSizes fields totalSize =
    let len = Array.length fields
    let get n =
      if n < len then Array.get fields n |> snd
      else totalSize
    let init idx = (get (idx + 1)) - (get idx)
    Array.init len init

  let isAlignedInt (_, cons, _) =
    match cons with
    | Int (size, _) when size % 8 = 0 -> true
    | _ -> false

  let isInts constTs = Array.exists (isAlignedInt >> not) constTs |> not

  let mkBigInt constTs =
    let map (_, cons, _) =
      match cons with
      | Int (_, bytes) -> bytes
      | _ -> failwith "mkBigInt fail"
    let bytes = Array.map map constTs |> Array.concat
    ofBytes ((Array.length bytes) * 8) bytes

  let rec liftConstT ctx (ty, cons, info) =
    let cache = LiftContext.getConstTCache ctx
    let add _ = doLiftConstT ctx (ty, cons, info)
    cache.GetOrAdd (ValueInfo.getPtr info, add)

  and doLiftConstT ctx (ty, cons, _) =
    match cons with
    | Int (size, bytes) -> ofBytes size bytes
    | Float (ty, bytes) -> ofBytes (Type.getFloatSize ty) bytes
    | Null ty
    | AggregateZero ty -> Type.getSize ty |> mkZero
    | Struct (structTy, fields) -> liftStruct ctx structTy fields
    | Constant.Array (_, constTs)
    | Vector constTs when isInts constTs -> mkBigInt constTs
    | Constant.Array (_, constTs)
    | Vector constTs ->
      mkConcat (Array.map (liftConstT ctx) constTs) (Type.getSize ty)
    | Undef ty -> mkUndef (Type.getSize ty)
    | BlockAddr (fID, bID) ->
      LiftContext.resolveBlockID ctx fID bID |> mkBlockAddr
    | Global (ty, (id, _)) -> mkGlobalVar id (Type.getSize ty)
    | TokenNone -> failwith "Not Supported yet"
    | Constant.BinOp (op, c1, c2) ->
      liftBinary op ty (liftConstT ctx c1) (liftConstT ctx c2)
    | Constant.ICmp (op, c1, c2) ->
      liftICmp op (ConstantT.getType c1) (liftConstT ctx c1) (liftConstT ctx c2)
    | Constant.FCmp (op, c1, c2) ->
      liftFCmp op (ConstantT.getType c1) (liftConstT ctx c1) (liftConstT ctx c2)
    | Constant.LogicOp (op, c1, c2) ->
      liftLogic op (ConstantT.getType c1) (liftConstT ctx c1) (liftConstT ctx c2)
    | Constant.Cast (op, cons, dstTy) ->
      liftCast op (liftConstT ctx cons) (ConstantT.getType cons) dstTy
    | Constant.GetElemPtr (_, addr, indices) ->
      Array.map (liftConstT ctx) indices
      |> liftGetElemPtr (ConstantT.getType addr) (liftConstT ctx addr)
    | Constant.Select (c, t, f) -> liftSelect ctx ty c t f
    | Constant.ExtractElem (vec, idx) ->
      liftExtractElem ty (liftConstT ctx vec) (liftConstT ctx idx)
    | Constant.InsertElem (dst, idx, src) ->
      liftInsertElem ty (liftConstT ctx dst) (liftConstT ctx idx)
                        (liftConstT ctx src)
    | Constant.Shuffle (v1, v2, mask) ->
      liftShuffle ty (liftConstT ctx v1) (liftConstT ctx v2) mask
    | Constant.ExtractVal (value, indices) ->
      liftExtractVal ty (liftConstT ctx value) indices
    | Constant.InsertVal (dst, indices, src) ->
      liftInsertVal ty (liftConstT ctx dst) indices (liftConstT ctx src)

  and liftStructMap ctx size field = mkZEXT (liftConstT ctx field) size

  and liftStruct ctx (_, fieldTys, size, _, _) fields =
    let fieldSizes = calcFieldSizes fieldTys size
    mkConcat (Array.map2 (liftStructMap ctx) fieldSizes fields) size

  and liftSelect ctx ty c t f =
    Type.getSize ty
    |> mkSelect (liftConstT ctx c) (liftConstT ctx t) (liftConstT ctx f)

  let convMetadata ctx = function
    | LocalAs (ty, n, _) -> LocalAs (ty, n, LiftContext.getVarID ctx n)
    | md -> md

  let getNxtMD ctx = function
    | LocalVar (_, r1)
    | DerivedType (_, _, r1, _)
    | SubProgram (_, _, r1, _) -> [|r1|]
    | CompositeType (_, _, _, r1s, r2s, _) -> Array.append r1s r2s
    | SubRoutine rs -> rs
    | _ -> [||]

  let liftMetadata ctx ptr md =
    let cache = LiftContext.getMDCache ctx
    let getter ptr = LiftContext.getMD ctx ptr |> convMetadata ctx
    let rec loop ptr =
      if cache.ContainsKey ptr then ()
      else
        let md = cache.GetOrAdd (ptr, getter)
        getNxtMD ctx md |> Array.iter loop
    loop ptr
    ptr, cache.TryGetValue ptr |> snd

  let liftOperand ctx = function
    | Argument (ty, idx, _) -> mkArg idx (Type.getSize ty)
    | Local (ty, name) ->mkVar (LiftContext.getVarID ctx name) (Type.getSize ty)
    | Operand.Constant constT -> liftConstT ctx constT
    | Metadata (ptr, md) -> liftMetadata ctx ptr md ||> Builder.mkMetadata

  let liftOperandOpt ctx = function
    | Some op -> liftOperand ctx op |> Some
    | None -> None

  let mkDef ctx name expr = [mkDef (LiftContext.getVarID ctx name) expr]

  let liftCondBr ctx cond tID fID =
    let cond = liftOperand ctx cond
    let tID = LiftContext.getBlockID ctx tID
    let fID = LiftContext.getBlockID ctx fID
    [mkCondJmp cond tID fID]

  let liftCase ctx (constT, name) =
    liftConstT ctx constT, LiftContext.getBlockID ctx name

  let liftSwitch ctx cond cases default_ =
    let cond = liftOperand ctx cond
    let cases = Array.map (liftCase ctx) cases
    let default_ = LiftContext.getBlockID ctx default_
    [mkSwitch cond cases default_]

  let liftIndBr ctx o targets =
    let o = liftOperand ctx o
    [Array.map (LiftContext.getBlockID ctx) targets |> mkIndJmp o]

  let liftCallee ctx = function
    | Operand (Constant (ty, Global (_, (id, _)), _)) ->
      Type.getRetTy ty, mkCalleeID id
    | Operand o ->
      Type.getRetTy (Operand.getType o), mkCalleeExpr (liftOperand ctx o)
    | InlineAsm (ty, code, cons) -> ty, mkInlineAsm code cons

  let liftInvoke ctx name callee args nID eID =
    let name = LiftContext.getVarID ctx name
    let nID = LiftContext.getBlockID ctx nID
    let eID = LiftContext.getBlockID ctx eID
    let args = Array.map (liftOperand ctx) args
    let retTy, callee = liftCallee ctx callee
    [mkTry name callee args (Type.getSize retTy) nID eID]

  let liftUnOp ctx name op o =
    let ty = Operand.getType o
    mkUnOp (liftUnOpType op) (liftOperand ctx o) (Type.getSize ty)
    |> mkDef ctx name

  let liftAlloca ctx name ty cnt size =
    let cnt = liftOperand ctx cnt
    let cntSize = getSize cnt
    let sz = mkInt size cntSize
    let e = mkMul sz cnt cntSize
    mkAlloca (LiftContext.getVarID ctx name) e

  let liftGetElemPtrInstr ctx name addr indices =
    Array.map (liftOperand ctx) indices
    |> liftGetElemPtr (Operand.getType addr) (liftOperand ctx addr)
    |> mkDef ctx name

  let liftPhiMap ctx (name, op) =
    LiftContext.getBlockID ctx name, liftOperand ctx op

  let liftPhi ctx name ty map =
    let size = Type.getSize ty
    let map = Map.toArray map |> Array.map (liftPhiMap ctx) |> Map.ofArray
    [mkPhi (LiftContext.getVarID ctx name) map size]

  let liftCall ctx name func args =
    let name = LiftContext.getVarID ctx name
    let args = Array.map (liftOperand ctx) args
    let retTy, callee = liftCallee ctx func
    [mkCall name callee args (Type.getSize retTy)]

  let liftSelectInstr ctx name c t f =
    let ty = Operand.getType t
    let c = liftOperand ctx c
    let t = liftOperand ctx t
    let f = liftOperand ctx f
    mkSelect c t f (Type.getSize ty) |> mkDef ctx name

  let liftExtractElemInstr ctx name vec idx =
    let ty = Operand.getType vec
    liftExtractElem ty (liftOperand ctx vec) (liftOperand ctx idx)
    |> mkDef ctx name

  let liftInsertElemInstr ctx name dst idx src =
    let ty = Operand.getType dst
    let dst = liftOperand ctx dst
    let idx = liftOperand ctx idx
    let src = liftOperand ctx src
    liftInsertElem ty dst idx src |> mkDef ctx name

  let liftShuffleInstr ctx name v1 v2 mask =
    let ty = Operand.getType v1
    let v1 = liftOperand ctx v1
    let v2 = liftOperand ctx v2
    liftShuffle ty v1 v2 mask |> mkDef ctx name

  let liftExtractValInstr ctx name v indices =
    let ty = Operand.getType v
    liftExtractVal ty (liftOperand ctx v) indices |> mkDef ctx name

  let liftInsertValInstr ctx name dst indices src =
    let ty = Operand.getType dst
    liftInsertVal ty (liftOperand ctx dst) indices (liftOperand ctx src)
    |> mkDef ctx name

  let liftCmpXchg ctx name addr cmp value =
    let size = Operand.getType cmp |> Type.getSize
    let addr = liftOperand ctx addr
    let mem = mkLoad addr size
    let cmp = liftOperand ctx cmp |> mkEq mem
    let value = mkSelect cmp mem (liftOperand ctx value) size
    let ret = mkConcat [|mem; mkSelect cmp True False 1|] (size + 1)
    (mkDef ctx name ret) @ [mkStore addr value]

  let liftMax v1 v2 size = mkSelect (mkSlt v1 v2) v2 v1 size

  let liftUMax v1 v2 size = mkSelect (mkUlt v1 v2) v2 v1 size

  let liftAtomicRMW ctx name op addr value =
    let ty = Operand.getType value
    let size = Type.getSize ty
    let addr = liftOperand ctx addr
    let mem = mkLoad addr size
    let ret = mkDef ctx name mem
    let name = LiftContext.getVarID ctx name
    let value = liftOperand ctx value
    let value =
      match op with
      | Xchg -> value
      | Add  -> mkAdd mem value size
      | Sub  -> mkSub mem value size
      | And  -> mkAnd mem value size
      | Nand -> mkNeg (mkAnd mem value size) size
      | Or   -> mkOr mem value size
      | Xor  -> mkXor mem value size
      | Max  -> liftMax mem value size
      | Min  -> liftMax value mem size
      | UMax -> liftUMax mem value size
      | UMin -> liftUMax value mem size
      | FAdd -> liftFAdd (Type.toFloatTy ty) mem value
      | FSub -> liftFSub (Type.toFloatTy ty) mem value
    (mkStore addr value) :: ret

  let liftInstr ctx ret (name, instr) =
    match instr with
    | Ret (arg, _) -> [liftOperandOpt ctx arg |> Return]
    | CondBr (cond, tID, fID, _) -> liftCondBr ctx cond tID fID
    | Br (name, _) -> [LiftContext.getBlockID ctx name |> mkJmp]
    | Switch (cond, cases, default_, _) -> liftSwitch ctx cond cases default_
    | IndirectBr (o, targets, _) -> liftIndBr ctx o targets
    | Invoke (callee, args, nID, eID, _) ->
      liftInvoke ctx name callee args nID eID
    | Unreachable _ -> [unreachable]
    | UnOp (op, o, _) -> liftUnOp ctx name op o
    | BinOp (op, o1, o2, _) ->
      liftBinary op (Operand.getType o1) (liftOperand ctx o1) (liftOperand ctx o2)
      |> mkDef ctx name
    | ICmp (op, o1, o2, _) ->
      liftICmp op (Operand.getType o1) (liftOperand ctx o1) (liftOperand ctx o2)
      |> mkDef ctx name
    | FCmp (op, o1, o2, _) ->
      liftFCmp op (Operand.getType o1) (liftOperand ctx o1) (liftOperand ctx o2)
      |> mkDef ctx name
    | LogicOp (op, o1, o2, _) ->
      liftLogic op (Operand.getType o1) (liftOperand ctx o1) (liftOperand ctx o2)
      |> mkDef ctx name
    | Alloca (ty, cnt, size, _) -> [liftAlloca ctx name ty cnt size]
    | Load (addr, size, _) ->
      mkLoad (liftOperand ctx addr) size |> mkDef ctx name
    | Store (addr, value, _, _) ->
      [mkStore (liftOperand ctx addr) (liftOperand ctx value)]
    | GetElemPtr (addr, indices, _, _) ->
      liftGetElemPtrInstr ctx name addr indices
    | Cast (castTy, op, dstTy, _) ->
      liftCast castTy (liftOperand ctx op) (Operand.getType op) dstTy
      |> mkDef ctx name
    | Phi (ty, map, _) -> liftPhi ctx name ty map
    | Call (func, args, _) -> liftCall ctx name func args
    | Select (c, t, f, _) -> liftSelectInstr ctx name c t f
    | ExtractElem (vec, idx, _) -> liftExtractElemInstr ctx name vec idx
    | InsertElem (dst, idx, src, _) -> liftInsertElemInstr ctx name dst idx src
    | Shuffle (v1, v2, mask, _) -> liftShuffleInstr ctx name v1 v2 mask
    | ExtractVal (src, indices, _) -> liftExtractValInstr ctx name src indices
    | InsertVal (dst, indices, src, _) ->
      liftInsertValInstr ctx name dst indices src
    | Fence _ -> [nop]
    | CmpXchg (addr, cmp, value, _, _, _) -> liftCmpXchg ctx name addr cmp value
    | AtomicRMW (op, addr, value, _, _) -> liftAtomicRMW ctx name op addr value
    | Resume (o, _) -> [liftOperand ctx o |> mkThrow]
    | LandingPad (ty, _, _, _, _) ->
      [mkCatch (LiftContext.getVarID ctx name) (Type.getSize ty)]
    | CleanupRet _
    | CatchRet _
    | CatchPad _
    | CleanupPad _
    | CatchSwitch _ -> failwith "Not Supported yet"
    @ ret

  let liftBlock ctx (_, instrs) =
    Array.fold (liftInstr ctx) [] instrs |> Array.revList

  let getNxtBlocks = function
    | Def _
    | Stmt.Alloca _
    | Stmt.Phi _
    | Stmt.Store _
    | Stmt.Call _
    | Catch _
    | NOP -> LiftException "getNxtBlocks" |>  raise
    | Try (_, _, _, _, nID, eID, _) -> [|nID; eID|]
    | Jmp id -> [|id|]
    | IndJmp (_, nxts) -> nxts
    | CondJmp (_, t, f) -> [|t; f|]
    | Stmt.Switch (_, cases, d) -> Array.append (Array.map snd cases) [|d|]
    | Throw _
    | Stmt.Return _
    | UnReachable -> [||]

  let calcEdge (idx, ret) block =
    match getNxtBlocks (Array.last block) with
    | [||] -> idx + 1, ret
    | arr -> idx + 1, Map.add idx (Set.ofArray arr) ret

  let liftArg idx (ty, _) = idx, Type.getSize ty

  let toFunctionMD ctx ptr = function
    | SubProgram (_, _, ty, _) ->
      match LiftContext.getMD ctx ty with
      | SubRoutine [||] -> ptr, Builder.emptyFuncMD
      | SubRoutine types ->
        let ret, args = Array.pop types
        let ret = if ret = 0 then None else LiftContext.getMD2 ctx ret |> Some
        let args = Array.map (LiftContext.getMD2 ctx) args
        ptr, Builder.mkFuncMD ret args
      | _ -> LiftException "toFunctionMD fail" |> raise
    | Metadata.TODO -> ptr, Builder.emptyFuncMD
    | _ -> LiftException "toFunctionMD fail" |> raise

  let liftFunc ctx ((id, _), func) = async {
    let blocks = func.Blocks
    let args = Function.getArgs func
    let ctx = LiftContext.addBlocks ctx blocks args
    let args =  Array.mapi liftArg args
    let body = Array.map (liftBlock ctx) blocks
    let nodes = Set.ofArray [|0.. (Array.length blocks) - 1|]
    let rets = Set.filter (Array.get body >> Array.exists Core.Stmt.isRet) nodes
    let edges = Array.fold calcEdge (0, Map.empty) body |> snd
    let md = func.Metadata ||> liftMetadata ctx ||> toFunctionMD ctx
    return id, mkFunc id args body nodes rets edges func.IsDeclr md
  }

  let liftGlobal ctx glob = async {
    let id = GlobalVar.getID glob |> fst
    let expr =
      match glob.Value with
      | Some consT -> liftConstT ctx consT
      | _ -> mkUndef (Type.getSize glob.Type)
    let md = glob.Metadata ||> liftMetadata ctx
    return id, Builder.mkGlobal id expr md glob.IsWritable
  }

  let private toInstrID (((fID, _), bidx), idx) = fID, bidx, idx

  let private convCaller (func, instrs) =
    Function.getID func |> fst, Set.map toInstrID instrs

  let private convCallee (instr, funcs) =
    toInstrID instr, Set.map (Function.getID >> fst) funcs

  let liftModule m =
    Logger.info "Lifting modules"
    let map f = async { return Function.getID f, f }
    let funcs = Module.getFunctions m |> doParallel map
    let globs = Module.getGlobals m
    let ctx = Module.getMetadatas m |> LiftContext.init (Map.ofArray funcs)
    {
      Functions = doParallel (liftFunc ctx) funcs |> Map.ofArray
      Globals = doParallel (liftGlobal ctx) globs |> Map.ofArray
      Metadatas = LiftContext.getMetadatas ctx
      CacheReduceMD = new CacheReduceMD ()
      CacheSubMD = new CacheSubMD ()
    } |> CallGraph.analyze

  let liftOnlyFuncs m =
    Logger.info "Start lifting only functions"
    let map f = Function.getID f, f
    let funcs = Module.getFunctions m |> Array.map map |> Map.ofArray
    let ctx = Module.getMetadatas m |> LiftContext.init funcs
    {
      Functions = Map.toArray funcs |> doParallel (liftFunc ctx) |> Map.ofArray
      Globals = Map.empty
      Metadatas = LiftContext.getMetadatas ctx
      CacheReduceMD = new CacheReduceMD ()
      CacheSubMD = new CacheSubMD ()
    }
