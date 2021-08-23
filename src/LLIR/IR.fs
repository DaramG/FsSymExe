namespace LLIR

open System.Collections.Concurrent

type ID = string

type BlockID = int

type VarID = BlockID * int

type UndefID = int

type ExprInfo = {
  Depth: int
}

type StartPos = int

type UnOpType =
  | NOT
  | NEG

type BinOpType =
  | ADD
  | SUB
  | MUL
  | UDIV
  | SDIV
  | UREM
  | SREM
  | AND
  | OR
  | XOR
  | SHL
  | LSHR
  | ASHR

type RelOpType =
  | EQ
  | NE
  | ULT
  | ULE
  | UGT
  | UGE
  | SLT
  | SLE
  | SGT
  | SGE

type CastType =
  | ZEXT
  | SEXT

type ExprSize = int

type Arg = int * ExprSize

type MD = LLVM.MetadataRef * LLVM.Metadata

type Expr =
  (* Base * Index *)
  | Addr of Expr * Expr * ExprInfo
  | Any of Expr [] * ExprSize
  | Arg of Arg
  | BinOp of BinOpType * Expr * Expr * ExprSize * ExprInfo
  | BlockAddr of ID * BlockID
  | Cast of CastType * Expr * ExprSize * ExprInfo
  | Concat of Expr [] * ExprSize * ExprInfo
  | Extract of Expr * Expr * ExprSize * ExprInfo
  | Num of BitVector
  | Load of Expr * ExprSize * ExprInfo
  | LocalAddr of int
  | RelOp of RelOpType * Expr * Expr * ExprInfo
  (* Dst * Pos * Src * Size *)
  | Replace of Expr * Expr * Expr * ExprSize * ExprInfo
  | Select of Expr * Expr * Expr * ExprSize * ExprInfo
  | SymVar of ID * ExprSize
  | Var of VarID * ExprSize
  | GlobalVar of ID * ExprSize
  | Undef of UndefID * ExprSize
  | Metadata of MD
  (* Uninterpreted Call *)
  | UnIntCall of ID * Expr [] * ExprSize * ExprInfo
  | UnOp of UnOpType * Expr * ExprSize * ExprInfo

type Callee =
  | ID of ID
  | Expr of Expr
  | InlineAsm of string * string

type Stmt =
  | Def of VarID * Expr
  | Alloca of VarID * Expr
  | Phi of VarID * Map<BlockID, Expr> * ExprSize * ExprInfo
  | Store of Expr * Expr
  | Call of VarID * Callee * Expr [] * ExprSize * ExprInfo
  | Try of VarID * Callee * Expr [] * ExprSize * BlockID * BlockID * ExprInfo
  | Throw of Expr * ExprInfo
  | Catch of VarID * ExprSize
  | Jmp of BlockID
  | IndJmp of Expr * BlockID []
  | CondJmp of Expr * BlockID * BlockID
  | Switch of Expr * (Expr * BlockID) [] * BlockID
  | Return of Expr option
  | UnReachable
  | NOP

type Block = Stmt []

type FunctionMD = {
  RetVal: MD option
  Args: MD []
}

type Function = {
  ID: ID
  Args: Arg []
  Body: Block []
  Nodes: Set<BlockID>
  RetNodes: Set<BlockID>
  Edges: Map<BlockID, Set<BlockID>>
  IsDeclr: bool
  Metadata: LLVM.MetadataRef * FunctionMD
  Calls: Map<VarID, ID [] option>
}

type GlobalVar = {
  ID: ID
  Value: Expr
  Metadata: MD
  IsWritable: bool
}

type InstrID = ID * BlockID * int

type CacheReduceMD = ConcurrentDictionary<LLVM.MetadataRef, LLVM.MetadataRef>

type CacheSubMD =
  ConcurrentDictionary<LLVM.MetadataRef * LLVM.MetadataRef, bool>

type Program = {
  Functions: Map<ID, Function>
  Globals: Map<ID, GlobalVar>
  Metadatas: LLVM.Metadatas
  CacheReduceMD: CacheReduceMD
  CacheSubMD: CacheSubMD
}
