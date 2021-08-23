namespace LLVM.Core

open LLVM
open Common

module ConstantT =
  let getType (ty, _, _) = ty

  let getConstant (_, cons, _) = cons

module ValueInfo =
  let getPtr (info: ValueInfo) = info.LLVMPtr

module CallableOperand =
  let getCalleeFuncID = function
    | Operand (Constant (_, Constant.Global (_, id), _)) -> Some id
    | _ -> None

  let isIndirect = function
    | Operand (Constant (_, Constant.Global _, _))
    | InlineAsm _ -> false
    | _ -> true

  let getGlobalID = function
    | Operand (Constant (_, Global (_, id), _)) -> Some id
    | _ -> None

module Type =
  let rec isBasic = function
    | Void _
    | Integer _
    | Type.Float _
    | X86Mmx _
    | Label _
    | Type.Metadata _
    | Token _ -> true
    | Function _
    | Type.Struct _
    | Named _ -> false
    | Type.Array (ty, _, _)
    | Type.Vector (ty, _, _)
    | Pointer (ty, _, _) -> isBasic ty

  let isVoid = function
    | Void _ -> true
    | _ -> false

  let isPtr = function
    | Pointer _ -> true
    | _ -> false

  let getArrayItem = function
    | Type.Array (ty, _, _) -> ty
    | _ -> failwith "getArrayItem fail"

  let isVector = function
    | Type.Vector _ -> true
    | _ -> false

  let isFunc = function
    | Function _ -> true
    | _ -> false

  let isFuncPtr = function
    | Pointer (ty, _, _) -> isFunc ty
    | _ -> false

  let getFuncTy = function
    | Pointer (Function (ty, _), _, _) -> Some ty
    | _ -> None

  let getStructTy = function
    | Pointer (Type.Struct (ty, _), _, _) -> Some ty
    | _ -> None

  let getRetTy = function
    | Pointer (Function ((ret, _, _), _), _, _) -> ret
    | _ -> failwith "getRetTy fail"

  (* Bit Size *)
  let getFloatSize = function
    | FloatTy.Half -> 16
    | FloatTy.Float -> 32
    | FloatTy.Double -> 64
    | FloatTy.X86Fp80 -> 80
    | FloatTy.Fp128   -> 128
    | FloatTy.PpcFp128 -> 128

  let getIntSize = function
    | Type.Integer (size, _) -> size
    | _ -> failwith "getIntSize fail"

  let rec getSize = function
    | Void _
    | Pointer _ -> 64
    | Integer (n, _) -> n
    | Type.Float (ty, _) -> getFloatSize ty
    | X86Mmx _ -> 128
    | Label _
    | Function _
    | Type.Metadata _
    | Type.Token _
    | Named _ -> 0//failwith "Not Supported"
    | Type.Struct ((_, _, size, _, _), _) -> size
    | Type.Array (ty, n, _)
    | Type.Vector (ty, n, _) -> (getSize ty) * n

  let getVectorSize = function
    | Type.Vector (ty, num, _) -> getSize ty, num
    | _ -> failwith "getVectorSize fail"

  let toFloatTy = function
    | Type.Float (ty, _) -> ty
    | _ -> failwith "toFloatTy fail"

  let isClassPtr = function
    | Pointer (Named ((id, _), _), _, _)
    | Pointer (Type.Struct (((id, _), _, _, _, _), _), _, _) ->
      id.StartsWith "class."
    | _ -> false

module Operand =
  let getType = function
    | Operand.Argument (ty, _, _) -> ty
    | Operand.Local (ty, _) -> ty
    | Operand.Constant (ty, _, _) -> ty
    | _ -> failwith "Not Supported yet"

  let isInt op =
    match getType op with
    | Type.Integer _ -> true
    | _ -> false

  let getSize op = getType op |> Type.getSize

module GlobalVar =
  let getID value = value.ID

module BasicBlock =
  let last (_, instrs) = Array.last instrs |> snd

  let getName (name, _) = name

module Function =
  let getType func = func.Type

  let getBB func = func.Blocks

  let getID (func: Function) = func.ID

  let getBody func = Array.map snd func.Blocks

  let isDeclr (func: Function) = func.IsDeclr

  let getBBNames func = Array.map fst func.Blocks

  let getArgs func = fst func.Parameters

module Module =
  let getName m = m.Name

  let getFunctions m = m.Functions

  let getGlobals m = m.GlobalVars

  let getTypes m = m.Types

  let getMetadatas m = m.Metadatas
