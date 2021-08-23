module LLVM.TypeChecker

open LLVM.Core

exception TypeCheckException of string

let checkConstantIndice = function
  | Type.Integer (32, _), Constant.Int _, _ -> ()
  | Type.Integer (64, _), _, _ -> ()
  | Type.Vector _, _, _ -> ()
  | _ -> TypeCheckException "checkConstantIndice fail" |> raise

let checkConstantIndices indices =
  Array.iter checkConstantIndice indices
  indices

let checkIndice = function
  | Constant cons -> checkConstantIndice cons
  | Local (Type.Integer (_, _), _)
  | Local (Type.Vector _, _)
  | Argument (Type.Integer (_, _), _, _)
  | Argument (Type.Vector _, _, _) -> ()
  | indice -> TypeCheckException "checkIndice fail" |> raise

let checkIndices indices =
  Array.iter checkIndice indices
  indices

let checkVectorOp op =
  match Operand.getType op with
  | Type.Vector _ -> op
  | _ -> TypeCheckException "checkVectorOp" |> raise

let checkInsertElem dst src =
  match Operand.getType dst with
  | Type.Vector (dstElemTy, _, _) when dstElemTy = Operand.getType src -> ()
  | _ -> TypeCheckException "checkInsertElem" |> raise

let isInt32OrUndef (_, cons, _) =
  match cons with
  | Constant.Int (32, _)
  | Undef (Type.Integer (32, _)) -> true
  | _ -> false

let checkShuffleMask = function
  | AggregateZero (Type.Vector (Type.Integer (32, _), _, _)) -> true
  | Constant.Vector vector -> Array.forall isInt32OrUndef vector
  | _ -> false

let checkShuffle o1 o2 (_, mask, _) =
  let o1 = Operand.getType o1
  let o2 = Operand.getType o2
  if checkShuffleMask mask && Type.isVector o1 && o1 = o2 then ()
  else TypeCheckException "checkShuffle" |> raise

let isPtr = function
  | Pointer _ -> true
  | _ -> false

let isInt = function
  | Integer _ -> true
  | _ -> false

let checkVector checkSrc checkDst srcTy dstTy =
  match srcTy, dstTy with
  | Type.Vector (srcTy, n, _), Type.Vector (dstTy, m, _) ->
    if n = m && checkSrc srcTy && checkDst dstTy then ()
    else TypeCheckException "checkVector fail" |> raise
  | srcTy, dstTy ->
    if checkSrc srcTy && checkDst dstTy then ()
    else TypeCheckException "checkVector fail" |> raise

let checkCast op srcTy dstTy =
  match op with
  | BitCast ->
    if Type.getSize srcTy = Type.getSize dstTy then ()
    else TypeCheckException "checkCast BitCast" |> raise
  | PtrToInt -> checkVector isPtr isInt srcTy dstTy
  | _ -> ()
