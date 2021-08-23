namespace LLVM

open System.Collections

type Name = string

type ID = Name * uint64

type LLVMPtr = uint64

[<Struct>]
type ConsInfo = {
  Hash: int
  LLVMPtr: LLVMPtr
}

[<Struct>]
type ValueInfo = {
  LLVMPtr: LLVMPtr
}

type FloatTy =
  | Half
  | Float
  | Double
  | X86Fp80
  | Fp128
  | PpcFp128

[<CustomEquality; CustomComparison>]
type Type =
  | Void of ConsInfo

  (* typeBits *)
  | Integer of int * ConsInfo
  | Float of FloatTy * ConsInfo
  | X86Mmx of ConsInfo
  | Label of ConsInfo
  | Function of FunctionTy * ConsInfo

  | Struct of StructTy * ConsInfo

  (* Type * nums *)
  | Array of Type * int * ConsInfo

  (* RefType * AddrSpace *)
  | Pointer of Type * int * ConsInfo

  (* Vector Type * nums *)
  | Vector of Type * int * ConsInfo
  | Metadata of ConsInfo
  | Token of ConsInfo
  | Named of ID * ConsInfo

  member __.GetConsInfo () =
    match __ with
    | Void x
    | Label x
    | Metadata x
    | Token x
    | Integer (_, x)
    | Float (_, x)
    | X86Mmx x
    | Named (_, x)
    | Function (_, x)
    | Struct (_, x)
    | Array (_, _, x)
    | Pointer (_, _, x)
    | Vector (_, _, x) -> x

  member private __.HasConsInfo () =
    let info = __.GetConsInfo ()
    info.Hash >= 0 || info.LLVMPtr > 0UL

  override __.Equals lhs =
    match lhs with
    | :? Type as x ->
      match __, x with
      | Void _, Void _
      | Label _, Label _
      | Metadata _, Metadata _
      | Token _, Token _
      | X86Mmx _, X86Mmx _ -> true
      | Integer (v1, _), Integer (v2, _) -> v1 = v2
      | Float (v1, _), Float (v2, _) -> v1 = v2
      | Named (v1, _), Named (v2, _) -> v1 = v2
      | t1, t2 when t1.HasConsInfo () && t2.HasConsInfo () ->
        let i1 = t1.GetConsInfo ()
        let i2 = t2.GetConsInfo ()
        i1.Hash = i2.Hash || i1.LLVMPtr = i2.LLVMPtr
      | Function (t1, _), Function (t2, _) -> t1 = t2
      | Struct (t1, _), Struct (t2, _) -> t1 = t2
      | Array (t1, n1, _), Array (t2, n2, _) -> n1 = n2 && t1 = t2
      | Pointer (t1, n1, _), Pointer (t2, n2, _) -> n1 = n2 && t1 = t2
      | Vector (t1, n1, _), Vector (t2, n2, _) -> n1 = n2 && t1 = t2
      | _ -> false
    | _ -> false

  override __.GetHashCode () =
    let consInfo = __.GetConsInfo ()
    if consInfo.Hash >= 0 then consInfo.Hash
    else failwithf "Invalid Type"

  interface System.IComparable with
    override __.CompareTo other =
      match other with
      | :? Type as other -> __.GetHashCode () - other.GetHashCode ()
      | _ -> failwith "Invalid comparison"

and FunctionTy = Type * Type [] * bool

and StructTy = ID * (Type * int) [] * int * int * bool

type UnOpType =
  | FNeg

type BinOpType =
  | Add
  | FAdd
  | Sub
  | FSub
  | Mul
  | FMul
  | UDiv
  | SDiv
  | FDiv
  | URem
  | SRem
  | FRem

type ICmpType =
  | Eq
  | Ne
  | Ugt
  | Uge
  | Ult
  | Ule
  | Sgt
  | Sge
  | Slt
  | Sle

type FCmpType =
  | False
  | Oeq
  | Ogt
  | Oge
  | Olt
  | Ole
  | One
  | Ord
  | Uno
  | Ueq
  | Ugt
  | Uge
  | Ult
  | Ule
  | Une
  | True

type LogicOpType =
  | Shl
  | LShr
  | AShr
  | And
  | Or
  | Xor

type CastType =
  | Trunc
  | ZExt
  | SExt
  | FPToUI
  | FPToSI
  | UIToFP
  | SIToFP
  | FPTrunc
  | FPExt
  | PtrToInt
  | IntToPtr
  | BitCast
  | AddrSpaceCast

type RMWOp =
  | Xchg
  | Add
  | Sub
  | And
  | Nand
  | Or
  | Xor
  | Max
  | Min
  | UMax
  | UMin
  | FAdd
  | FSub

type Constant =
  | Int of int * byte []
  | Float of FloatTy * byte []
  | Null of Type
  | AggregateZero of Type
  | Struct of StructTy * ConstantT []
  | Array of Type * ConstantT []
  | Vector of ConstantT []
  | Undef of Type
  | BlockAddr of Name * Name
  | Global of Type * ID
  | TokenNone
  | BinOp of BinOpType * ConstantT * ConstantT
  | ICmp of ICmpType * ConstantT * ConstantT
  | FCmp of FCmpType * ConstantT * ConstantT
  | LogicOp of LogicOpType * ConstantT * ConstantT
  | Cast of CastType * ConstantT * Type
  | GetElemPtr of bool * ConstantT * ConstantT []
  | Select of ConstantT * ConstantT * ConstantT
  | ExtractElem of ConstantT * ConstantT
  | InsertElem of ConstantT * ConstantT * ConstantT
  | Shuffle of ConstantT * ConstantT * ConstantT
  | ExtractVal of ConstantT * int []
  | InsertVal of ConstantT * int [] * ConstantT

and ConstantT = Type * Constant * ValueInfo

type MetadataRef = int

type MDDerivedTypeBaseKind =
  | Member
  | TypeDef
  | Inherit
  | Friend
  | Ptr
  | PtrToMember
  | Ref
  | Const
  | Volatile
  | Restrict
  | Atomic
  | RValRef

type MDCompositeTypeKind =
  | Class
  | Structure
  | Array
  | Union
  | Enum

type Metadata =
  (* Name * Type *)
  | LocalVar of Name * MetadataRef
  (* Kind * Name * BaseType * Offset*)
  | DerivedType of MDDerivedTypeBaseKind * Name * MetadataRef * int
  (* Kind * Name * ID * TemplateParams * Elements *)
  | CompositeType of MDCompositeTypeKind * Name * Name * MetadataRef [] * MetadataRef [] * int
  (* Type * Name *)
  | LocalAs of Type * Name * (int * int)
  (* Name * LinkName * VirtualIndex *)
  | SubProgram of Name * Name * MetadataRef * int option
  | SubRoutine of MetadataRef []
  (* Name * Size *)
  | BasicType of Name * int
  | VOID
  | TODO

type Metadatas = Map<MetadataRef, Metadata>

type Operand =
  | Argument of Type * int * ValueInfo
  | Local of Type * Name
  | Constant of ConstantT
  | Metadata of MetadataRef * Metadata

type CallableOperand =
  | Operand of Operand
  | InlineAsm of Type * string * string

type SyncScope =
  | SingleThread
  | System

type MemoryOrder =
  | NotAtomic
  | Unordered
  | Monotonic
  | Acquire
  | Release
  | AcquireRelease
  | SeqConsistent

type Atomicity = SyncScope * MemoryOrder

type Instruction =
  | Ret of Operand option * ValueInfo
  | CondBr of Operand * Name * Name * ValueInfo
  | Br of Name * ValueInfo
  | Switch of Operand * (ConstantT * Name) [] * Name * ValueInfo
  | IndirectBr of Operand * Name [] * ValueInfo

  (* Func * Args * RetDest * ExceptDest *)
  | Invoke of CallableOperand * Operand [] * Name * Name * ValueInfo
  | Unreachable of ValueInfo

  | UnOp of UnOpType * Operand * ValueInfo
  | BinOp of BinOpType * Operand * Operand * ValueInfo
  | ICmp of ICmpType * Operand * Operand * ValueInfo
  | FCmp of FCmpType * Operand * Operand * ValueInfo
  | LogicOp of LogicOpType * Operand * Operand * ValueInfo

  (* Allocated Type * NumElements * ElemSize *)
  | Alloca of Type * Operand * int * ValueInfo

  (* Address * BitSize *)
  | Load of Operand * int * ValueInfo

  (* Address * Value * align *)
  | Store of Operand * Operand * uint32 * ValueInfo

  (* Address * Indices * InBounds *)
  | GetElemPtr of Operand * Operand [] * bool * ValueInfo
  | Cast of CastType * Operand * Type * ValueInfo
  | Phi of Type * Map<Name, Operand> * ValueInfo
  | Call of CallableOperand * Operand [] * ValueInfo
  | Select of Operand * Operand * Operand * ValueInfo
  (* Skip UserOp1, Userop2 *)
  | ExtractElem of Operand * Operand * ValueInfo
  | InsertElem of Operand * Operand * Operand * ValueInfo
  | Shuffle of Operand * Operand * ConstantT * ValueInfo
  | ExtractVal of Operand * int [] * ValueInfo
  | InsertVal of Operand * int [] * Operand * ValueInfo
  | Fence of Atomicity * ValueInfo
  | CmpXchg of Operand * Operand * Operand * Atomicity * MemoryOrder * ValueInfo
  | AtomicRMW of RMWOp * Operand * Operand * Atomicity * ValueInfo
  | Resume of Operand * ValueInfo
  | LandingPad of Type * ConstantT [] * ConstantT [] * bool * ValueInfo
  | CleanupRet of Operand * Name option * ValueInfo
  | CatchRet of Operand * Name * ValueInfo
  | CatchPad of Operand * Operand [] * ValueInfo
  | CleanupPad of Operand * Operand [] * ValueInfo
  | CatchSwitch of Operand * Name [] * Name * ValueInfo

type Parameter = Type * Name

type BasicBlock = Name * (Name * Instruction) []

type FuncType =
  | Method
  | Constructor
  | Destructor
  | Normal

type Function = {
  ID: ID
  Type: FunctionTy
  (* Parameter [] * isVarArg  *)
  Parameters: Parameter [] * bool
  Blocks: BasicBlock []
  LLVMPtr: LLVMPtr
  IsDeclr: bool
  Metadata: MetadataRef * Metadata
}

type GlobalVar = {
  ID: ID
  Type: Type
  Value: ConstantT option
  Metadata: MetadataRef * Metadata
  IsWritable: bool
}

type Module = {
  Name: Name
  GlobalVars: GlobalVar []
  Functions: Function []
  Types: Map<ID, Type>
  LLVMPtr: LLVMPtr
  Metadatas: Metadatas
}
