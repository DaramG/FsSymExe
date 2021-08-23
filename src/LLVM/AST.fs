namespace LLVM

open System.Collections
open LLVM.Core
open LLVM.TypeChecker
open System.Runtime.InteropServices

module AST =
  [<DllImport("llvm\libllvm.dll")>]
  extern string demangle (string);

  let private tyHashTable = new HashTable<Type> ()
  let private ptrTyTable = new Concurrent.ConcurrentDictionary<Type, Type> ()
  let private namedTyTable = new Concurrent.ConcurrentDictionary<uint64, Type> ()

  let resolveNamedTy name =
    let (_, ptr), _ = name
    namedTyTable.GetOrAdd (ptr, (fun ptr -> tyHashTable.Get(Named name)))

  let updateHash info h = { info with Hash = h }

  let updatePtr (info: ConsInfo) ptr = { info with LLVMPtr = ptr }

  let private tyFactory h = function
    | Void info -> Void (updateHash info h)
    | Type.Integer (x, info) -> Type.Integer (x, updateHash info h)
    | Type.Float (x, info) -> Type.Float (x, updateHash info h)
    | X86Mmx info -> X86Mmx (updateHash info h)
    | Label info -> Label (updateHash info h)
    | Type.Metadata info -> Type.Metadata (updateHash info h)
    | Token info -> Token (updateHash info h)
    | Named (x, info) -> Named (x, updateHash info h)
    | Function (x, info) -> Function (x, updateHash info h)
    | Type.Struct (x, info) -> Type.Struct (x, updateHash info h)
    | Type.Array (x1, x2, info) -> Type.Array (x1, x2, updateHash info h)
    | Type.Pointer (x1, x2, info) -> Type.Pointer (x1, x2, updateHash info h)
    | Type.Vector (x1, x2, info) -> Type.Vector (x1, x2, updateHash info h)

  let private addPtr ptr = function
    | Void info -> Void (updatePtr info ptr)
    | Type.Integer (x, info) -> Type.Integer (x, updatePtr info ptr)
    | Type.Float (x, info) -> Type.Float (x, updatePtr info ptr)
    | X86Mmx info -> X86Mmx (updatePtr info ptr)
    | Label info -> Label (updatePtr info ptr)
    | Type.Metadata info -> Type.Metadata (updatePtr info ptr)
    | Token info -> Token (updatePtr info ptr)
    | Named (x, info) -> Named (x, updatePtr info ptr)
    | Function (x, info) -> Function (x, updatePtr info ptr)
    | Type.Struct (x, info) -> Type.Struct (x, updatePtr info ptr)
    | Type.Array (x1, x2, info) -> Type.Array (x1, x2, updatePtr info ptr)
    | Type.Pointer (x1, x2, info) -> Type.Pointer (x1, x2, updatePtr info ptr)
    | Type.Vector (x1, x2, info) -> Type.Vector (x1, x2, updatePtr info ptr)

  let calcTyHash ty ptr =
    let ty = addPtr ptr ty
    match ty with
    | Named _ -> ty
    | ty -> tyHashTable.AddHash (tyFactory, ty)

  let emptyInfo = { Hash = -1; LLVMPtr = 0UL }

  let voidTy = Type.Void emptyInfo

  let halfFloatTy = FloatTy.Half

  let floatFloatTy = FloatTy.Float

  let doubleFloatTy = FloatTy.Double

  let x86fp80FloatTy =  FloatTy.X86Fp80

  let fp128FloatTy = FloatTy.Fp128

  let ppcfp128FloatTy = FloatTy.PpcFp128

  let halfTy = Type.Float (FloatTy.Half, emptyInfo)

  let floatTy = Type.Float (FloatTy.Float, emptyInfo)

  let doubleTy = Type.Float (FloatTy.Double, emptyInfo)

  let x86fp80Ty = Type.Float (FloatTy.X86Fp80, emptyInfo)

  let fp128Ty = Type.Float (FloatTy.Fp128, emptyInfo)

  let ppcfp128Ty = Type.Float (FloatTy.PpcFp128, emptyInfo)

  let x86mmxTy = Type.X86Mmx emptyInfo

  let labelTy= emptyInfo |> Type.Label

  let intTy width = Type.Integer (width, emptyInfo)

  let funcTy retTy paramTys isVaArg =
    Type.Function ((retTy, paramTys, isVaArg), emptyInfo)

  let structTy ty = Type.Struct (ty, emptyInfo)

  let arrTy ty nums = Type.Array (ty, nums, emptyInfo)

  let ptrTy ty space = Type.Pointer (ty, space, emptyInfo)

  let private mkPtrTyHelper ty =
    tyHashTable.GetOrAddHash (tyFactory, ptrTy ty 0)

  let mkPtrTy ty = ptrTyTable.GetOrAdd (ty, mkPtrTyHelper)

  let vecTy ty nums = Type.Vector (ty, nums, emptyInfo)

  let metaTy= emptyInfo |> Type.Metadata

  let tokenTy= emptyInfo |> Type.Token

  let namedTy (name, ptr) = calcTyHash (Type.Named ((name, ptr), emptyInfo)) ptr

  let todoMD = 0, Metadata.TODO

  let mkMDLocalVar name ty = Metadata.LocalVar (name, ty)

  let mkMDLocalAs ty name = Metadata.LocalAs (ty, name, (-1, -1))

  let mkMDDerivedType kind name ty offset =
    Metadata.DerivedType (kind, name, ty, offset)

  let mkMDCompositeType kind name id params_ elems offset =
    Metadata.CompositeType (kind, name, id, params_, elems, offset)

  let mkMDSubProgram name link ty vidx =
    Metadata.SubProgram (name, link, ty, vidx)

  let mkMDSubRoutine refs =  Metadata.SubRoutine refs

  let mkMDBasicType name size = Metadata.BasicType (name, size)

  let mkInlineAsm ty code cons = CallableOperand.InlineAsm (ty, code, cons)

  let mkNull ty = Constant.Null ty

  let mkUndef ty = Constant.Undef ty

  let mkAggregateZero ty = Constant.AggregateZero ty

  let mkValueInfo ptr = { LLVMPtr = ptr }

  let mkUndefT ty = ty, Constant.Undef ty, mkValueInfo 0UL

  let mkConstantT ty cons ptr = (ty, cons, mkValueInfo ptr)

  let mkConstantInt size arr = Constant.Int (size, arr)

  let mkConstantFloat ty bytes = Constant.Float (ty, bytes)

  let mkConstantStruct ty cons = Constant.Struct (ty, cons)

  let mkConstantArr ty arr = Constant.Array (ty, arr)

  let mkConstantVec arr = Constant.Vector arr

  let mkConstantGlobal ty name = Constant.Global (ty, name)

  let mkConstantBinOp op c1 c2 = Constant.BinOp (op, c1, c2)

  let mkConstantICmp op c1 c2 = Constant.ICmp (op, c1, c2)

  let mkConstantFCmp op c1 c2 = Constant.FCmp (op, c1, c2)

  let mkConstantLogicOp op c1 c2 = Constant.LogicOp (op, c1, c2)

  let mkConstantCast op cons ty =
    checkCast op (ConstantT.getType cons) ty
    Constant.Cast (op, cons, ty)

  let mkConstantGetElemPtr inBound addr indices =
    Constant.GetElemPtr (inBound, addr, checkConstantIndices indices)

  let mkConstantSelect cond tCons fCons = Constant.Select (cond, tCons, fCons)

  let mkConstantShuffle c1 c2 mask = Constant.Shuffle (c1, c2, mask)

  let mkConstantExtractVal dst indices = Constant.ExtractVal (dst, indices)

  let mkConstantExtractElem dst idx = Constant.ExtractElem (dst, idx)

  let mkConstantInsertVal dst src indices =
    Constant.InsertVal (dst, indices, src)

  let mkConstantInsertElem dst src idx = Constant.InsertElem (dst, idx, src)

  let mkBlockAddr n1 n2 = Constant.BlockAddr (n1, n2)

  let tokenNone  = Constant.TokenNone

  let mkRet ptr op = Instruction.Ret (op, mkValueInfo ptr)

  let mkBr ptr name = Instruction.Br (name, mkValueInfo ptr)

  let mkCondBr ptr cond tBr fBr =
    Instruction.CondBr (cond, tBr, fBr, mkValueInfo ptr)

  let mkSwitch ptr cond cases _default =
    Instruction.Switch (cond, cases, _default, mkValueInfo ptr)

  let mkPhi ptr ty ops names =
    Instruction.Phi (ty, Array.zip names ops |> Map.ofArray, mkValueInfo ptr)

  let mkSelect ptr cond tOp fOp =
    Instruction.Select (cond, tOp, fOp, mkValueInfo ptr)

  let mkIndirectBr ptr arg names =
    Instruction.IndirectBr (arg, names, mkValueInfo ptr)

  let mkInvoke ptr callee args normal unwind =
    Instruction.Invoke (callee, args, normal, unwind, mkValueInfo ptr)

  let unreachable ptr = Instruction.Unreachable (mkValueInfo ptr)

  let mkUnOp ptr op o1 = Instruction.UnOp (op, o1, mkValueInfo ptr)

  let mkBinOp ptr op o1 o2 = Instruction.BinOp (op, o1, o2, mkValueInfo ptr)

  let mkICmp ptr op o1 o2 = Instruction.ICmp (op, o1, o2, mkValueInfo ptr)

  let mkFCmp ptr op o1 o2 = Instruction.FCmp (op, o1, o2, mkValueInfo ptr)

  let mkLogicOp ptr op o1 o2 = Instruction.LogicOp (op, o1, o2, mkValueInfo ptr)

  let mkCast ptr op o1 ty =
    checkCast op (Operand.getType o1) ty
    Instruction.Cast (op, o1, ty, mkValueInfo ptr)

  let mkLocal ty name = Operand.Local (ty, name)

  let mkArg ptr ty idx = Operand.Argument (ty, idx, mkValueInfo ptr)

  let mkAlloca ptr ty nums size =
    Instruction.Alloca (ty, nums, size, mkValueInfo ptr)

  let mkStore ptr addr value align =
    Instruction.Store (addr, value, align, mkValueInfo ptr)

  let mkLoad ptr addr size = Instruction.Load (addr, size, mkValueInfo ptr)

  let mkCall ptr callee args = Instruction.Call (callee, args, mkValueInfo ptr)

  let mkGetElemPtr ptr inBound addr indices =
    Instruction.GetElemPtr (addr, checkIndices indices, inBound, mkValueInfo ptr)

  let mkShuffle ptr o1 o2 mask =
    checkShuffle o1 o2 mask
    Instruction.Shuffle (o1, o2, mask, mkValueInfo ptr)

  let mkExtractVal ptr dst indices =
    Instruction.ExtractVal (dst, indices, mkValueInfo ptr)

  let mkExtractElem ptr dst idx =
    Instruction.ExtractElem (checkVectorOp dst, idx, mkValueInfo ptr)

  let mkInsertVal ptr dst src indices =
    Instruction.InsertVal (dst, indices, src, mkValueInfo ptr)

  let mkInsertElem ptr dst src idx =
    checkInsertElem dst src
    Instruction.InsertElem (dst, idx, src, mkValueInfo ptr)

  let mkLandingPad ptr ty catches filters cleanUp =
    Instruction.LandingPad (ty, catches, filters, cleanUp, mkValueInfo ptr)

  let mkFence ptr atomicity = Instruction.Fence (atomicity, mkValueInfo ptr)

  let mkCmpXchg ptr addr expected replace atomicity failorder =
    Instruction.CmpXchg (addr, expected, replace, atomicity, failorder,
                         mkValueInfo ptr)

  let mkAtomicRMW ptr op addr value atomicity =
    Instruction.AtomicRMW (op, addr, value, atomicity, mkValueInfo ptr)

  let mkResume ptr arg = Instruction.Resume (arg, mkValueInfo ptr)

  let mkGlobal name ty cons writable md = {
    ID = name
    Type = ty
    Value = cons
    Metadata = md
    IsWritable  = writable
  }

  let mkFunc ptr id ty params_ blocks isDeclr md = {
    ID = id
    Type = ty
    Parameters = params_
    Blocks = blocks
    LLVMPtr = ptr
    IsDeclr = isDeclr
    Metadata = md
  }

  let mkModule ptr name globals funcs types metadatas = {
    Name = name
    GlobalVars = globals
    Functions = funcs
    Types = types
    LLVMPtr = ptr
    Metadatas = metadatas
  }
