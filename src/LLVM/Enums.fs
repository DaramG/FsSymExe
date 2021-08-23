namespace LLVM

exception EnumsException of string

type TypeID =
  | ID_Void      = 0
  | ID_Half      = 1
  | ID_Float     = 2
  | ID_Double    = 3
  | ID_X86_FP80  = 4
  | ID_FP128     = 5
  | ID_PPC_FP128 = 6
  | ID_Label     = 7
  | ID_Integer   = 8
  | ID_Function  = 9
  | ID_Struct    = 10
  | ID_Array     = 11
  | ID_Pointer   = 12
  | ID_Vector    = 13
  | ID_Metadata  = 14
  | ID_X86_MMX   = 15
  | ID_Token     = 16

type OperandID =
  | ID_Argument    = 0
  | ID_Local       = 1
  | ID_Constant    = 2
  | ID_Metadata    = 3

type MetadataID =
  | ID_DILocalVariable = 0
  | ID_DIDerivedType = 1
  | ID_DICompositeType = 2
  | ID_LocalAsMetadata = 3
  | ID_SubProgram = 4
  | ID_SubRoutine = 5
  | ID_BasicType = 6
  | ID_Void = 7
  | ID_TODO = 8

type MDDerivedTypeTag =
  | ID_Member      = 0
  | ID_Typedef     = 1
  | ID_Inherit     = 2
  | ID_Friend      = 3
  | ID_Ptr         = 4
  | ID_PtrToMember = 5
  | ID_Ref         = 6
  | ID_Const       = 7
  | ID_Volatile    = 8
  | ID_Restrict    = 9
  | ID_Atomic      = 10
  | ID_RValRef     = 11

type MDCompositeTypeTag =
  | ID_Class     = 0
  | ID_Structure = 1
  | ID_Array     = 2
  | ID_Union     = 3
  | ID_Enum      = 4

type ConstantID =
  | ID_Int           = 0x100
  | ID_Half          = 0x101
  | ID_Single        = 0x102
  | ID_Double        = 0x103
  | ID_Quadruple     = 0x104
  | ID_X86Fp80       = 0x105
  | ID_PpcFp128      = 0x106
  | ID_Null          = 0x107
  | ID_AggregateZero = 0x108
  | ID_Struct        = 0x109
  | ID_Array         = 0x10a
  | ID_Vector        = 0x10b
  | ID_Undef         = 0x10c
  | ID_BlockAddr     = 0x10d
  | ID_Global        = 0x10e
  | ID_TokenNone     = 0x10f

type InstrID =
  (* Terminator Instructions *)
  | ID_Ret             = 0
  | ID_Br              = 1
  | ID_Switch          = 2
  | ID_IndirectBr      = 3
  | ID_Invoke          = 4
  | ID_Unreachable     = 5
  | ID_CallBr          = 6

  (* Standard Unary Operators *)
  | ID_FNeg            = 7

  (* Standard Binary Operators *)
  | ID_Add             = 8
  | ID_FAdd            = 9
  | ID_Sub             = 10
  | ID_FSub            = 11
  | ID_Mul             = 12
  | ID_FMul            = 13
  | ID_UDiv            = 14
  | ID_SDiv            = 15
  | ID_FDiv            = 16
  | ID_URem            = 17
  | ID_SRem            = 18
  | ID_FRem            = 19

  (* Logical Operators *)
  | ID_Shl             = 20
  | ID_LShr            = 21
  | ID_AShr            = 22
  | ID_And             = 23
  | ID_Or              = 24
  | ID_Xor             = 25

  (* Memory Operators *)
  | ID_Alloca          = 26
  | ID_Load            = 27
  | ID_Store           = 28
  | ID_GetElementPtr   = 29

  (* Cast Operators *)
  | ID_Trunc           = 30
  | ID_ZExt            = 31
  | ID_SExt            = 32
  | ID_FPToUI          = 33
  | ID_FPToSI          = 34
  | ID_UIToFP          = 35
  | ID_SIToFP          = 36
  | ID_FPTrunc         = 37
  | ID_FPExt           = 38
  | ID_PtrToInt        = 39
  | ID_IntToPtr        = 40
  | ID_BitCast         = 41
  | ID_AddrSpaceCast   = 42

  (* Other Operators *)
  | ID_ICmp            = 43
  | ID_FCmp            = 44
  | ID_PHI             = 45
  | ID_Call            = 46
  | ID_Select          = 47
  | ID_UserOp1         = 48
  | ID_UserOp2         = 49
  | ID_VAArg           = 50
  | ID_ExtractElement  = 51
  | ID_InsertElement   = 52
  | ID_ShuffleVector   = 53
  | ID_ExtractValue    = 54
  | ID_InsertValue     = 55
  | ID_Fence           = 56
  | ID_AtomicCmpXchg   = 57
  | ID_AtomicRMW       = 58
  | ID_Resume          = 59
  | ID_LandingPad      = 60
  | ID_CleanupRet      = 61
  | ID_CatchRet        = 62
  | ID_CatchPad        = 63
  | ID_CleanupPad      = 64
  | ID_CatchSwitch     = 65
  | ID_Freeze          = 66

type SyncScopeID =
  | ID_SingleThread = 0
  | ID_System       = 1

type MemoryOrderID =
  | ID_NotAtomic      = 0
  | ID_Unordered      = 1
  | ID_Monotonic      = 2
  | ID_Acquire        = 3
  | ID_Release        = 4
  | ID_AcquireRelease = 5
  | ID_SeqConsistent  = 6

type RMWOpID =
  | ID_Xchg          = 0
  | ID_Add           = 1
  | ID_Sub           = 2
  | ID_And           = 3
  | ID_Nand          = 4
  | ID_Or            = 5
  | ID_Xor           = 6
  | ID_Max           = 7
  | ID_Min           = 8
  | ID_UMax          = 9
  | ID_UMin          = 10
  | ID_FAdd          = 11
  | ID_FSub          = 12

type CmpID =
  | ID_FALSE = 0
  | ID_OEQ   = 1
  | ID_OGT   = 2
  | ID_OGE   = 3
  | ID_OLT   = 4
  | ID_OLE   = 5
  | ID_ONE   = 6
  | ID_ORD   = 7
  | ID_UNO   = 8
  | ID_UEQ   = 9
  | ID_UGT   = 10
  | ID_UGE   = 11
  | ID_ULT   = 12
  | ID_ULE   = 13
  | ID_UNE   = 14
  | ID_TRUE  = 15
  | ID_Eq    = 16
  | ID_Ne    = 17
  | ID_Ugt   = 18
  | ID_Uge   = 19
  | ID_Ult   = 20
  | ID_Ule   = 21
  | ID_Sgt   = 22
  | ID_Sge   = 23
  | ID_Slt   = 24
  | ID_Sle   = 25

module InstrID =
  let toUnOpType = function
    | InstrID.ID_FNeg -> UnOpType.FNeg
    | _ -> EnumsException "toUnOpType fail" |> raise

  let toBinOpType = function
    | InstrID.ID_Add  -> BinOpType.Add
    | InstrID.ID_FAdd -> BinOpType.FAdd
    | InstrID.ID_Sub  -> BinOpType.Sub
    | InstrID.ID_FSub -> BinOpType.FSub
    | InstrID.ID_Mul  -> BinOpType.Mul
    | InstrID.ID_FMul -> BinOpType.FMul
    | InstrID.ID_UDiv -> BinOpType.UDiv
    | InstrID.ID_SDiv -> BinOpType.SDiv
    | InstrID.ID_FDiv -> BinOpType.FDiv
    | InstrID.ID_URem -> BinOpType.URem
    | InstrID.ID_SRem -> BinOpType.SRem
    | InstrID.ID_FRem -> BinOpType.FRem
    | _ -> EnumsException "toBinOpType fail" |> raise

  let toLogicOpType = function
    | InstrID.ID_Shl  -> LogicOpType.Shl
    | InstrID.ID_LShr -> LogicOpType.LShr
    | InstrID.ID_AShr -> LogicOpType.AShr
    | InstrID.ID_And  -> LogicOpType.And
    | InstrID.ID_Or   -> LogicOpType.Or
    | InstrID.ID_Xor  -> LogicOpType.Xor
    | _ -> EnumsException "toLogicOpType fail" |> raise

  let toCastType = function
    | InstrID.ID_Trunc         -> CastType.Trunc
    | InstrID.ID_ZExt          -> CastType.ZExt
    | InstrID.ID_SExt          -> CastType.SExt
    | InstrID.ID_FPToUI        -> CastType.FPToUI
    | InstrID.ID_FPToSI        -> CastType.FPToSI
    | InstrID.ID_UIToFP        -> CastType.UIToFP
    | InstrID.ID_SIToFP        -> CastType.SIToFP
    | InstrID.ID_FPTrunc       -> CastType.FPTrunc
    | InstrID.ID_FPExt         -> CastType.FPExt
    | InstrID.ID_PtrToInt      -> CastType.PtrToInt
    | InstrID.ID_IntToPtr      -> CastType.IntToPtr
    | InstrID.ID_BitCast       -> CastType.BitCast
    | InstrID.ID_AddrSpaceCast -> CastType.AddrSpaceCast
    | _ -> EnumsException "toCastType fail" |> raise

module SyncScopeID =
  let toSyncScope = function
    | SyncScopeID.ID_SingleThread -> SyncScope.SingleThread
    | SyncScopeID.ID_System       -> SyncScope.System
    | _ -> EnumsException "toSyncScope fail" |> raise

module MemoryOrderID =
  let toMemoryOrder = function
    | MemoryOrderID.ID_NotAtomic      -> MemoryOrder.NotAtomic
    | MemoryOrderID.ID_Unordered      -> MemoryOrder.Unordered
    | MemoryOrderID.ID_Monotonic      -> MemoryOrder.Monotonic
    | MemoryOrderID.ID_Acquire        -> MemoryOrder.Acquire
    | MemoryOrderID.ID_Release        -> MemoryOrder.Release
    | MemoryOrderID.ID_AcquireRelease -> MemoryOrder.AcquireRelease
    | MemoryOrderID.ID_SeqConsistent  -> MemoryOrder.SeqConsistent
    | _ -> EnumsException "toMemoryOrder fail" |> raise

module RMWOpID =
  let toRMWOp = function
    | RMWOpID.ID_Xchg -> RMWOp.Xchg
    | RMWOpID.ID_Add -> RMWOp.Add
    | RMWOpID.ID_Sub -> RMWOp.Sub
    | RMWOpID.ID_And -> RMWOp.And
    | RMWOpID.ID_Nand -> RMWOp.Nand
    | RMWOpID.ID_Or -> RMWOp.Or
    | RMWOpID.ID_Xor -> RMWOp.Xor
    | RMWOpID.ID_Max -> RMWOp.Max
    | RMWOpID.ID_Min -> RMWOp.Min
    | RMWOpID.ID_UMax -> RMWOp.UMax
    | RMWOpID.ID_UMin -> RMWOp.UMin
    | RMWOpID.ID_FAdd -> RMWOp.FAdd
    | RMWOpID.ID_FSub -> RMWOp.FSub
    | _ -> EnumsException "toRMWOp fail" |> raise

module CmpID =
  let toICmpType = function
    | CmpID.ID_Eq  -> ICmpType.Eq
    | CmpID.ID_Ne  -> ICmpType.Ne
    | CmpID.ID_Ugt -> ICmpType.Ugt
    | CmpID.ID_Uge -> ICmpType.Uge
    | CmpID.ID_Ult -> ICmpType.Ult
    | CmpID.ID_Ule -> ICmpType.Ule
    | CmpID.ID_Sgt -> ICmpType.Sgt
    | CmpID.ID_Sge -> ICmpType.Sge
    | CmpID.ID_Slt -> ICmpType.Slt
    | CmpID.ID_Sle -> ICmpType.Sle
    | _ -> EnumsException "toICmpType fail" |> raise

  let toFCmpType = function
    | CmpID.ID_FALSE -> FCmpType.False
    | CmpID.ID_OEQ   -> FCmpType.Oeq
    | CmpID.ID_OGT   -> FCmpType.Ogt
    | CmpID.ID_OGE   -> FCmpType.Oge
    | CmpID.ID_OLT   -> FCmpType.Olt
    | CmpID.ID_OLE   -> FCmpType.Ole
    | CmpID.ID_ONE   -> FCmpType.One
    | CmpID.ID_ORD   -> FCmpType.Ord
    | CmpID.ID_UNO   -> FCmpType.Uno
    | CmpID.ID_UEQ   -> FCmpType.Ueq
    | CmpID.ID_UGT   -> FCmpType.Ugt
    | CmpID.ID_UGE   -> FCmpType.Uge
    | CmpID.ID_ULT   -> FCmpType.Ult
    | CmpID.ID_ULE   -> FCmpType.Ule
    | CmpID.ID_UNE   -> FCmpType.Une
    | CmpID.ID_TRUE  -> FCmpType.True
    | _ -> EnumsException "toFCmpType fail" |> raise

module MDDerivedTypeTag =
  let toKind = function
    | MDDerivedTypeTag.ID_Member -> MDDerivedTypeBaseKind.Member
    | MDDerivedTypeTag.ID_Typedef -> MDDerivedTypeBaseKind.TypeDef
    | MDDerivedTypeTag.ID_Inherit -> MDDerivedTypeBaseKind.Inherit
    | MDDerivedTypeTag.ID_Friend -> MDDerivedTypeBaseKind.Friend
    | MDDerivedTypeTag.ID_Ptr -> MDDerivedTypeBaseKind.Ptr
    | MDDerivedTypeTag.ID_PtrToMember -> MDDerivedTypeBaseKind.PtrToMember
    | MDDerivedTypeTag.ID_Ref -> MDDerivedTypeBaseKind.Ref
    | MDDerivedTypeTag.ID_Const -> MDDerivedTypeBaseKind.Const
    | MDDerivedTypeTag.ID_Volatile -> MDDerivedTypeBaseKind.Volatile
    | MDDerivedTypeTag.ID_Restrict -> MDDerivedTypeBaseKind.Restrict
    | MDDerivedTypeTag.ID_Atomic -> MDDerivedTypeBaseKind.Atomic
    | MDDerivedTypeTag.ID_RValRef -> MDDerivedTypeBaseKind.RValRef
    | _ -> EnumsException "MDDerivedTypeTag.toKind fail" |> raise

module MDCompositeTypeTag =
  let toKind = function
    | MDCompositeTypeTag.ID_Class -> MDCompositeTypeKind.Class
    | MDCompositeTypeTag.ID_Structure -> MDCompositeTypeKind.Structure
    | MDCompositeTypeTag.ID_Array -> MDCompositeTypeKind.Array
    | MDCompositeTypeTag.ID_Union -> MDCompositeTypeKind.Union
    | MDCompositeTypeTag.ID_Enum -> MDCompositeTypeKind.Enum
    | _ -> EnumsException "MDCompositeTypeTag.toKind fail" |> raise
