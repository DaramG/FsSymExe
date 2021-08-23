module LLVM.Reader

open LLVM.Context
open LLVM.Core
open Common
open System.Collections
open System.Runtime.InteropServices

exception ReaderException of string

[<DllImport("llvm\libllvm.dll")>]
extern void dump (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getContext ();

[<DllImport("llvm\libllvm.dll")>]
extern string ptrToString (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern string demangle (string);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr parseBitCode (CPtr, string);

[<DllImport("llvm\libllvm.dll")>]
extern string getModuleName (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getFuncCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getFuncs (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getBlockCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getBlocks (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getParamCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getParams (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getParamTyCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getParamTys (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getStructElemCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getStructElems (CPtr, CPtr, CPtr [], CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getStructAllocSize (CPtr, CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getInstrCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getInstrs (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getValueType (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern bool hasVarArg (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern bool isPacked (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern uint32 getArgNo (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getPtrAddrSpc (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getElemType (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getIntTypeWidth (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern string getValueName (CPtr, CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern string getStructName (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getRetType (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getVecSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getArrLen (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern TypeID getTypeID (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern InstrID getInstrID (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getRet (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getBr (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getAlloca (CPtr, CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getStore (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getLoad (CPtr, CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getUnOp (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getBinOp (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getTriple (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getShuffle (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getCast (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getCallSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getCall (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getGetElemPtrSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getGetElemPtr (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr consToInstr (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getSwitchSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getSwitch (CPtr, CPtr[], CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getPhiSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getPhi (CPtr, CPtr[], CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getConstantArrSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getConstantArr (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getConstantVecSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getConstantVec (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getConstantStructSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getConstantStruct (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getGlobalSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getGlobals (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getGlobal (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern bool isInlineAsm (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern bool isCleanUp (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern OperandID getOperandID (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getMetadata (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getMetadataLocalVar (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getMetadataLocalAs (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getMDDerivedType (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getMDCompositeType (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getMDSubProgram (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getMDArrayCnt (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getMDArray (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern MDCompositeTypeTag getMDCompositeTypeTag (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getMDSubRoutine (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern string getMDBasicTypeName (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getMDBasicTypeSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getFuncMD (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getGlobalMD (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern MetadataID getMetadataID (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern ConstantID getConstantID (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getConstantIntSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getConstantInt (CPtr, byte[]);

[<DllImport("llvm\libllvm.dll")>]
extern void getBytes (CPtr, byte [], int);

[<DllImport("llvm\libllvm.dll")>]
extern int getIndiceSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getIndices (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getExtract (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getInsert (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern string getInlineAsmCode (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern string getInlineAsmConstraint (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getInvokeDsts (CPtr, CPtr[]);

[<DllImport("llvm\libllvm.dll")>]
extern int getLandingPadSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getLandingPad (CPtr, CPtr [], CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getBlockAddr (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern int getOperandSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern void getOperands (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getFence (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getCmpXchAtomicity (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern void getRMWAtomicity (CPtr, CPtr []);

[<DllImport("llvm\libllvm.dll")>]
extern RMWOpID getRMWOp (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern bool isConstantStr (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern int getConstantStrSize (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CPtr getConstantStr (CPtr, byte [], int);

[<DllImport("llvm\libllvm.dll")>]
extern bool isWritable (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CmpID getConstantPred (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern CmpID getInstrPred (CPtr);

[<DllImport("llvm\libllvm.dll")>]
extern bool isDeclr (CPtr);

let private (=>) (ctx, x) f = ctx, f x

let getStructElemsHelper ctx (ptr, r1, r2) =
  getStructElems (Context.getModule ctx, ptr, r1, r2)

let filterNull arr = Array.filter (fun x -> x <> 0UL) arr

let getPtrArray getSize getPtr ptr =
  let ret = Array.create (getSize ptr) 0UL
  getPtr (ptr, ret)
  ret

let getPtrArray2 getSize getPtr ptr =
  let size = getSize ptr
  let ret1 = Array.create size 0UL
  let ret2 = Array.create size 0UL
  getPtr (ptr, ret1, ret2)
  ret1, ret2

let getByteArray getSize getVal ptr =
  let size = getSize ptr
  let ret = Array.create size 0uy
  let addr = getVal (ptr, ret, size)
  addr, ret

let prepare2 f ptr =
  let ret = [| 0UL; 0UL |]
  f (ptr, ret)
  ret.[0], ret.[1]

let prepare3 f ptr =
  let ret = [| 0UL; 0UL; 0UL |]
  f (ptr, ret)
  ret.[0], ret.[1], ret.[2]

let prepare4 f ptr =
  let ret = [| 0UL; 0UL; 0UL; 0UL |]
  f (ptr, ret)
  ret.[0], ret.[1], ret.[2], ret.[3]

let prepare5 f ptr =
  let ret = [| 0UL; 0UL; 0UL; 0UL; 0UL |]
  f (ptr, ret)
  ret.[0], ret.[1], ret.[2], ret.[3], ret.[4]

let prepare6 f ptr =
  let ret = [| 0UL; 0UL; 0UL; 0UL; 0UL; 0UL |]
  f (ptr, ret)
  ret.[0], ret.[1], ret.[2], ret.[3], ret.[4], ret.[5]

let getValName ctx ptr = getValueName (Context.getTrack ctx, ptr)

let getStructSize ctx ptr =
  let ret = [| 0UL; 0UL |]
  getStructAllocSize (Context.getModule ctx, ptr, ret)
  int ret.[0], int ret.[1]

let readBinary read conv ctx ptr =
  let o1, o2 = prepare2 getBinOp ptr
  let ctx, o1 = read ctx o1
  let ctx, o2 = read ctx o2
  ctx, conv o1 o2

let readTriple read conv ctx ptr =
  let o1, o2, o3 = prepare3 getTriple ptr
  let ctx, o1 = read ctx o1
  let ctx, o2 = read ctx o2
  let ctx, o3 = read ctx o3
  ctx, conv o1 o2 o3

let readGetElemPtr read conv ctx ptr =
  let inBound, addr, indices =
    getPtrArray getGetElemPtrSize getGetElemPtr ptr |> Array.pop2
  let inBound = if inBound = 0UL then false else true
  let ctx, addr = read ctx addr
  let ctx, indices = Array.mapState ctx read indices
  ctx, conv inBound addr indices

let readExtractVal read conv ctx ptr =
  let ctx, dst = getExtract ptr |> read ctx
  let indices = getPtrArray getIndiceSize getIndices ptr |> Array.map int
  ctx, conv dst indices

let rec readType ctx ptr =
  match Context.getType ctx ptr with
  | true, ty -> ctx, ty
  | _ -> readTypeFromPtr ctx ptr

and readTypeFromPtr ctx ptr =
  let addType = Context.addType ctx ptr
  match getTypeID ptr with
  | TypeID.ID_Void -> addType AST.voidTy
  | TypeID.ID_Half -> addType AST.halfTy
  | TypeID.ID_Float -> addType AST.floatTy
  | TypeID.ID_Double -> addType AST.doubleTy
  | TypeID.ID_X86_FP80 -> addType AST.x86fp80Ty
  | TypeID.ID_FP128 -> addType AST.fp128Ty
  | TypeID.ID_PPC_FP128 -> addType AST.ppcfp128Ty
  | TypeID.ID_Label -> addType AST.labelTy
  | TypeID.ID_Integer -> getIntTypeWidth ptr |> AST.intTy |> addType
  | TypeID.ID_Function -> readFuncTy ctx ptr
  | TypeID.ID_Struct -> readStructTy ctx ptr
  | TypeID.ID_Array -> readArrayTy ctx ptr
  | TypeID.ID_Pointer -> readPtrTy ctx ptr
  | TypeID.ID_Vector -> readVecTy ctx ptr
  | TypeID.ID_Metadata -> addType AST.metaTy
  | TypeID.ID_X86_MMX -> addType AST.x86mmxTy
  | TypeID.ID_Token -> addType AST.tokenTy
  | _ -> ReaderException "readType fail" |> raise

and readFuncTy ctx ptr =
  let ctx, retTy = getRetType ptr |> readType ctx
  let ctx, params_ = getPtrArray getParamTyCnt getParamTys ptr
                     |> Array.mapState ctx readType
  AST.funcTy retTy params_ (hasVarArg ptr)
  |> Context.addType ctx ptr

and readStructElems ctx ptr =
  let elems, idxs = getPtrArray2 getStructElemCnt (getStructElemsHelper ctx) ptr
  let ctx, elems = Array.mapState ctx readType elems
  ctx, Array.zip elems (Array.map int idxs)

and readStructTy ctx ptr =
  let id = getStructName ptr, ptr
  if Context.containsTyID ctx id then ctx, AST.namedTy id
  else
    let ctx, elems = readStructElems (Context.pushTyID ctx id) ptr
    let size, align = getStructSize ctx ptr
    AST.structTy (id, elems, size, align, isPacked ptr)
    |> Context.addType (Context.popTyID ctx) ptr

and readArrayTy ctx ptr =
  let ctx, ty = getElemType ptr |> readType ctx
  AST.arrTy ty (getArrLen ptr) |> Context.addType ctx ptr

and readPtrTy ctx ptr =
  let ctx, ty = getElemType ptr |> readType ctx
  AST.ptrTy ty (getPtrAddrSpc ptr) |> Context.addType ctx ptr

and readVecTy ctx ptr =
  let ctx, ty = getElemType ptr |> readType ctx
  AST.vecTy ty (getVecSize ptr) |> Context.addType ctx ptr

let getValType ctx ptr = getValueType ptr |> readType ctx

let readInsertVal read convUndef conv ctx ptr =
  let dst, src = prepare2 getInsert ptr
  let indices = getPtrArray getIndiceSize getIndices ptr |> Array.map int
  let ctx, dst = read ctx dst
  let ctx, src =
    if src = 0UL then getValType ctx ptr => convUndef
    else read ctx src
  ctx, conv dst src indices

let getFuncTy ctx ptr =
  match readType ctx ptr with
  | ctx, Pointer (Type.Function (funcTy, _), _, _) -> ctx, funcTy
  | _ -> ReaderException "getFuncTy fail" |> raise

let getValStructTy ctx ptr =
  match getValType ctx ptr with
  | ctx, Type.Struct (ty, _) -> ctx, ty
  | _ -> ReaderException "getValStructTy fail" |> raise

let readCast read conv ctx id ptr =
  let op = InstrID.toCastType id
  let ty, o1 = prepare2 getCast ptr
  let ctx, ty = readType ctx ty
  let ctx, o1 = read ctx o1
  ctx, conv op o1 ty

let readParam ctx ptr =
  let ctx, ty = getValType ctx ptr
  ctx, (ty, getValName ctx ptr)

let readParams ctx ptr =
  getPtrArray getParamCnt getParams ptr |> Array.mapState ctx readParam

let readArg ctx ptr =
  let ctx, ty  = getValType ctx ptr
  let idx = getArgNo ptr |> int
  ctx, AST.mkArg ptr ty idx

let readLocal ctx ptr =
  let ctx, ty = getValType ctx ptr
  let name = getValName ctx ptr
  ctx, AST.mkLocal ty name

let readConstantInt ptr =
  let size = getConstantIntSize ptr
  let bsize = (size + 7) / 8
  let arr = Array.create bsize 0uy
  getConstantInt (ptr, arr)
  AST.mkConstantInt size arr

let byteToConstantT ty (addr, ret) b =
  addr + 1UL, AST.mkConstantT ty (AST.mkConstantInt 8 [|b|]) addr :: ret

let readConstantGlobal ctx ptr =
  let ctx, ty = getValType ctx ptr
  ctx, AST.mkConstantGlobal ty (getValName ctx ptr, ptr)

let readBlockAddr ctx ptr =
  let pre = Context.getFuncPtr ctx
  let func, block = prepare2 getBlockAddr ptr
  let ctx = Context.setFuncPtr ctx func
  Context.setFuncPtr ctx pre,
  AST.mkBlockAddr (getValName ctx func) (getValName ctx block)

let readFloat ty size ptr =
  let arr = Array.create size 0uy
  getBytes (ptr, arr, size)
  AST.mkConstantFloat ty arr

let readConstantICmpType ptr = getConstantPred ptr |> CmpID.toICmpType

let readICmpType ptr = getInstrPred ptr |> CmpID.toICmpType

let readConstantFCmpType ptr = getConstantPred ptr |> CmpID.toFCmpType

let readFCmpType ptr = getInstrPred ptr |> CmpID.toFCmpType

let rec readConstantT ctx ptr : Context * ConstantT =
  match Context.getConstantT ctx ptr with
  | true, t -> ctx, t
  | _ ->
    let ctx, ty = getValType ctx ptr
    let ctx, cons = readConstantFromPtr ctx ptr
    AST.mkConstantT ty cons ptr |> Context.addConstantT ctx ptr

and readConstantFromPtr ctx ptr =
  match getConstantID ptr with
  | ConstantID.ID_Int -> ctx, readConstantInt ptr
  | ConstantID.ID_Half -> ctx, readFloat AST.halfFloatTy 2 ptr
  | ConstantID.ID_Single -> ctx, readFloat AST.floatFloatTy 4 ptr
  | ConstantID.ID_Double -> ctx, readFloat AST.doubleFloatTy 8 ptr
  | ConstantID.ID_Quadruple -> ctx, readFloat AST.fp128FloatTy 8 ptr
  | ConstantID.ID_X86Fp80 -> ctx, readFloat AST.x86fp80FloatTy 10 ptr
  | ConstantID.ID_PpcFp128 -> ctx, readFloat AST.ppcfp128FloatTy 8 ptr
  | ConstantID.ID_Null -> getValType ctx ptr => AST.mkNull
  | ConstantID.ID_AggregateZero -> getValType ctx ptr => AST.mkAggregateZero
  | ConstantID.ID_Struct -> readConstantStruct ctx ptr
  | ConstantID.ID_Array -> readConstantArray ctx ptr
  | ConstantID.ID_Vector -> readConstantVector ctx ptr
  | ConstantID.ID_Undef -> getValType ctx ptr => AST.mkUndef
  | ConstantID.ID_BlockAddr -> readBlockAddr ctx ptr
  | ConstantID.ID_Global -> readConstantGlobal ctx ptr
  | ConstantID.ID_TokenNone -> ctx, AST.tokenNone
  | id -> readConstantExpr ctx ptr (enum<InstrID> (int id))

and readConstantStruct ctx ptr =
  let ctx, ty = getValStructTy ctx ptr
  getPtrArray getConstantStructSize getConstantStruct ptr
  |> Array.mapState ctx readConstantT => (AST.mkConstantStruct ty)

and readConstantArray ctx ptr =
  let ctx, ty = getValType ctx ptr
  if isConstantStr ptr then
    let byteTy = Type.getArrayItem ty
    let addr, arr = getByteArray getConstantStrSize getConstantStr ptr
    let arr = Array.fold (byteToConstantT byteTy) (addr, []) arr
              |> snd |> Array.revList |> AST.mkConstantArr ty
    ctx, arr
  else getPtrArray getConstantArrSize getConstantArr ptr
       |> Array.mapState ctx readConstantT => (AST.mkConstantArr ty)

and readConstantVector ctx ptr =
  getPtrArray getConstantVecSize getConstantVec ptr
  |> Array.mapState ctx readConstantT => AST.mkConstantVec

and readConstantExpr ctx ptr id =
  match id with
  | InstrID.ID_Add
  | InstrID.ID_FAdd
  | InstrID.ID_Sub
  | InstrID.ID_FSub
  | InstrID.ID_Mul
  | InstrID.ID_FMul
  | InstrID.ID_UDiv
  | InstrID.ID_SDiv
  | InstrID.ID_FDiv
  | InstrID.ID_URem
  | InstrID.ID_SRem
  | InstrID.ID_FRem ->
    readConstantBinary (InstrID.toBinOpType id |> AST.mkConstantBinOp) ctx ptr
  | InstrID.ID_ICmp ->
    readConstantBinary (readConstantICmpType ptr |> AST.mkConstantICmp) ctx ptr
  | InstrID.ID_FCmp ->
    readConstantBinary (readConstantFCmpType ptr |> AST.mkConstantFCmp) ctx ptr

  | InstrID.ID_Shl
  | InstrID.ID_LShr
  | InstrID.ID_AShr
  | InstrID.ID_And
  | InstrID.ID_Or
  | InstrID.ID_Xor ->
    readConstantBinary (InstrID.toLogicOpType id |> AST.mkConstantLogicOp) ctx ptr

  | InstrID.ID_Trunc
  | InstrID.ID_ZExt
  | InstrID.ID_SExt
  | InstrID.ID_FPToUI
  | InstrID.ID_FPToSI
  | InstrID.ID_UIToFP
  | InstrID.ID_SIToFP
  | InstrID.ID_FPTrunc
  | InstrID.ID_FPExt
  | InstrID.ID_PtrToInt
  | InstrID.ID_IntToPtr
  | InstrID.ID_BitCast
  | InstrID.ID_AddrSpaceCast ->
    readCast readConstantT AST.mkConstantCast ctx id ptr
  | InstrID.ID_GetElementPtr ->
    consToInstr ptr |> readGetElemPtr readConstantT AST.mkConstantGetElemPtr ctx
  | InstrID.ID_Select ->
    consToInstr ptr |> readTriple readConstantT AST.mkConstantSelect ctx
  | InstrID.ID_ExtractElement ->
    readBinary readConstantT AST.mkConstantExtractElem ctx ptr
  | InstrID.ID_InsertElement ->
    readTriple readConstantT AST.mkConstantInsertElem ctx ptr
  | InstrID.ID_ShuffleVector ->
    readTriple readConstantT AST.mkConstantShuffle ctx ptr
  | InstrID.ID_ExtractValue ->
    readExtractVal readConstantT AST.mkConstantExtractVal ctx ptr
  | InstrID.ID_InsertValue ->
    readInsertVal readConstantT AST.mkUndefT AST.mkConstantInsertVal ctx ptr
  | _ ->
    printfn "%A" id
    ReaderException "readConstantExpr fail" |> raise

and readConstantBinary = readBinary readConstantT

let readConstantOpt ctx = function
  | 0UL -> ctx, None
  | ptr -> readConstantT ctx ptr => Some

let readMDLocalAs ctx ptr =
  let local = getMetadataLocalAs ptr
  let ctx, ty = getValType ctx local
  let name = getValName ctx local
  AST.mkMDLocalAs ty name |> Context.addMetadata ctx ptr

let readMDBasicType ctx ptr =
  let name = getMDBasicTypeName ptr
  let size = getMDBasicTypeSize ptr
  AST.mkMDBasicType name size |> Context.addMetadata ctx ptr

let handleVoid = function
  | 0UL -> 1UL
  | ptr -> ptr

let rec readMetadata ctx ptr =
  if Context.hasMetadata ctx ptr then ctx
  else readMDFromPtr (Context.reserveMD ctx ptr) ptr

and readMetadatas ctx ptrs =
  let ctx = Array.fold readMetadata ctx ptrs
  ctx, Array.map (Context.toMDRef ctx) ptrs

and readMDFromPtr ctx ptr =
  match getMetadataID ptr with
  | MetadataID.ID_DILocalVariable -> readMDLocalVar ctx ptr
  | MetadataID.ID_DIDerivedType -> readMDDerivedType ctx ptr
  | MetadataID.ID_DICompositeType -> readMDCompositeType ctx ptr
  | MetadataID.ID_LocalAsMetadata -> readMDLocalAs ctx ptr
  | MetadataID.ID_SubProgram -> readMDSubProgram ctx ptr
  | MetadataID.ID_SubRoutine -> readMDSubRoutine ctx ptr
  | MetadataID.ID_BasicType -> readMDBasicType ctx ptr
  | MetadataID.ID_Void -> Context.addMetadata ctx ptr Metadata.VOID
  | _ -> Context.addMetadata ctx ptr Metadata.TODO

and readMDLocalVar ctx ptr =
  let name, ty = prepare2 getMetadataLocalVar ptr
  let name = ptrToString name
  let ctx = readMetadata ctx ty
  let ty = Context.toMDRef ctx ty
  AST.mkMDLocalVar name ty |> Context.addMetadata ctx ptr

and readMDDerivedType ctx ptr =
  let tag, name, ty, offset = prepare4 getMDDerivedType ptr
  let kind = enum<MDDerivedTypeTag> (int32 tag) |> MDDerivedTypeTag.toKind
  let name = ptrToString name
  let offset = int offset
  let ctx = readMetadata ctx ty
  let ty = Context.toMDRef ctx ty
  AST.mkMDDerivedType kind name ty offset |> Context.addMetadata ctx ptr

and readMDCompositeType ctx ptr =
  let tag, name, id, params_, elems, offset = prepare6 getMDCompositeType ptr
  let kind = enum<MDCompositeTypeTag> (int32 tag) |> MDCompositeTypeTag.toKind
  let name = ptrToString name
  let id = ptrToString id
  let params_ = getPtrArray getMDArrayCnt getMDArray params_
  let elems = getPtrArray getMDArrayCnt getMDArray elems
  let ctx, params_ = readMetadatas ctx params_
  let ctx, elems = readMetadatas ctx elems
  AST.mkMDCompositeType kind name id params_ elems (int offset)
  |> Context.addMetadata ctx ptr

and readMDSubProgram ctx ptr =
  let name, link, ty, idx = prepare4 getMDSubProgram ptr
  let name = ptrToString name
  let link = ptrToString link
  let idx = if idx = (uint64 -1) then None else Some (int idx)
  let ctx = readMetadata ctx ty
  let ty = Context.toMDRef ctx ty
  AST.mkMDSubProgram name link ty idx |> Context.addMetadata ctx ptr

and readMDSubRoutine ctx ptr =
  let refs = getMDSubRoutine ptr |> getPtrArray getMDArrayCnt getMDArray
             |> Array.map handleVoid
  let ctx, refs = readMetadatas ctx refs
  AST.mkMDSubRoutine refs |> Context.addMetadata ctx ptr

let loadMetadata ctx ptr =
  let ctx = readMetadata ctx ptr
  ctx, (Context.toMDRef ctx ptr, Context.getMetadata ctx ptr)

let readOperandFromPtr ctx ptr =
  match getOperandID ptr with
  | OperandID.ID_Argument -> readArg ctx ptr
  | OperandID.ID_Local -> readLocal ctx ptr
  | OperandID.ID_Constant -> readConstantT ctx ptr => Operand.Constant
  | OperandID.ID_Metadata ->
    getMetadata ptr |> loadMetadata ctx => Operand.Metadata
  | _ -> ReaderException "readOperand fail" |> raise
  |> Context.addOperand ptr

let readOperand ctx ptr =
  match Context.getOperand ctx ptr with
  | true, operand -> ctx, operand
  | _ -> readOperandFromPtr ctx ptr

let readInlineAsm ctx ptr =
  let ctx, ty = getValType ctx ptr
  let code = getInlineAsmCode ptr
  let cons = getInlineAsmConstraint ptr
  ctx, AST.mkInlineAsm ty code cons

let readCallableOperand ctx ptr =
  if isInlineAsm ptr then readInlineAsm ctx ptr
  else readOperand ctx ptr => CallableOperand.Operand

let readRet ctx ptr =
  match getRet ptr with
  | 0UL -> ctx, AST.mkRet ptr None
  | ptr -> readOperand ctx ptr => (Some >> AST.mkRet ptr)

let readBr ctx ptr =
  match prepare3 getBr ptr with
  | 0UL, br, 0UL -> ctx, AST.mkBr ptr (getValName ctx br)
  | cond, tBr, fBr ->
    let ctx, cond = readOperand ctx cond
    ctx, AST.mkCondBr ptr cond (getValName ctx tBr) (getValName ctx fBr)

let readSwitch ctx ptr =
  let (cond, cases), (dBlock, cBlocks) =
    getPtrArray2 getSwitchSize getSwitch ptr |> Tuple.map Array.pop
  let ctx, cond = readOperand ctx cond
  let ctx, cases = Array.mapState ctx readConstantT cases
  let dName = getValName ctx dBlock
  let names = Array.map (getValName ctx) cBlocks
  ctx, AST.mkSwitch ptr cond (Array.zip cases names) dName

let readUnOp conv ctx ptr =
  let o1 = getUnOp ptr
  let ctx, o1 = readOperand ctx o1
  ctx, conv o1

let readInstrBinary = readBinary readOperand

let callGetAlloca ctx (ptr, ret) = getAlloca (Context.getModule ctx, ptr, ret)

let readAlloca ctx ptr =
  let ty, num, size = prepare3 (callGetAlloca ctx) ptr
  let ctx, ty = readType ctx ty
  let ctx, num = readOperand ctx num
  ctx, AST.mkAlloca ptr ty num (int size)

let callGetLoad ctx (ptr, ret) = getLoad (Context.getModule ctx, ptr, ret)

let readLoad ctx ptr =
  let addr, size = prepare2 (callGetLoad ctx) ptr
  let ctx, addr = readOperand ctx addr
  ctx, AST.mkLoad ptr addr (int size)

let readStore ctx ptr =
  let addr, value, align = prepare3 getStore ptr
  let ctx, addr = readOperand ctx addr
  let ctx, value = readOperand ctx value
  ctx, AST.mkStore ptr addr value (uint32 align)

let readPhi ctx ptr =
  let (ty, values), (_, blocks) =
    getPtrArray2 getPhiSize getPhi ptr |> Tuple.map Array.pop
  let ctx, ty = readType ctx ty
  let ctx, values = Array.mapState ctx readOperand values
  let names = Array.map (getValName ctx) blocks
  ctx, AST.mkPhi ptr ty values names

let readCall ctx ptr =
  let callee, args = getPtrArray getCallSize getCall ptr |> Array.pop
  let ctx, callee = readCallableOperand ctx callee
  let ctx, args = Array.mapState ctx readOperand args
  ctx, AST.mkCall ptr callee args

let readShuffle ctx ptr =
  let o1, o2, o3 = prepare3 getShuffle ptr
  let ctx, o1 = readOperand ctx o1
  let ctx, o2 = readOperand ctx o2
  let ctx, o3 = readConstantT ctx o3
  ctx, AST.mkShuffle ptr o1 o2 o3

let readIndirectBr ctx ptr =
  let arg, names = getPtrArray getOperandSize getOperands ptr |> Array.pop
  let names = Array.map (getValName ctx) names
  let ctx, arg = readOperand ctx arg
  ctx, AST.mkIndirectBr ptr arg names

let readInvoke ctx ptr =
  let callee, args = getPtrArray getCallSize getCall ptr |> Array.pop
  let ctx, callee = readCallableOperand ctx callee
  let ctx, args = Array.mapState ctx readOperand args
  let normal, unwind = prepare2 getInvokeDsts ptr |> Tuple.map (getValName ctx)
  ctx, AST.mkInvoke ptr callee args normal unwind

let readLandingPad ctx ptr =
  let ctx, ty = getValType ctx ptr
  let catches, filters = getPtrArray2 getLandingPadSize getLandingPad ptr
  let ctx, catches = filterNull catches |> Array.mapState ctx readConstantT
  let ctx, filters = filterNull filters |> Array.mapState ctx readConstantT
  let cleanUp = isCleanUp ptr
  ctx, AST.mkLandingPad ptr ty catches filters cleanUp

let readAtomicity getter ptr =
  let sync, order = prepare2 getter ptr
  let sync = SyncScopeID.toSyncScope (enum<SyncScopeID>(int sync))
  let order = MemoryOrderID.toMemoryOrder (enum<MemoryOrderID>(int order))
  sync, order

let readCmpXchg ctx ptr =
  let sync, succ, fail = prepare3 getCmpXchAtomicity ptr
  let sync = SyncScopeID.toSyncScope (enum<SyncScopeID>(int sync))
  let succ = MemoryOrderID.toMemoryOrder (enum<MemoryOrderID>(int succ))
  let fail = MemoryOrderID.toMemoryOrder (enum<MemoryOrderID>(int fail))
  let addr, expected, replace = prepare3 getTriple ptr
  let ctx, addr = readOperand ctx addr
  let ctx, expected = readOperand ctx expected
  let ctx, replace = readOperand ctx replace
  ctx, AST.mkCmpXchg ptr addr expected replace (sync, succ) fail

let readAtmoicRMW ctx ptr =
  let op = getRMWOp ptr |> RMWOpID.toRMWOp
  let atomicity = readAtomicity getRMWAtomicity ptr
  let addr, value = prepare2 getBinOp ptr
  let ctx, addr = readOperand ctx addr
  let ctx, value = readOperand ctx value
  ctx, AST.mkAtomicRMW ptr op addr value atomicity

let readInstr ctx ptr =
  let addName (ctx, instr) =
    ctx, (getValName ctx ptr, instr)
  let id = getInstrID ptr
  match id with
  | InstrID.ID_Ret -> readRet ctx ptr
  | InstrID.ID_Br -> readBr ctx ptr
  | InstrID.ID_Switch -> readSwitch ctx ptr
  | InstrID.ID_IndirectBr -> readIndirectBr ctx ptr
  | InstrID.ID_Invoke -> readInvoke ctx ptr
  | InstrID.ID_Unreachable -> ctx, AST.unreachable ptr
  | InstrID.ID_CallBr -> failwithf "todo %A" id

  | InstrID.ID_FNeg -> readUnOp (AST.mkUnOp ptr (InstrID.toUnOpType id)) ctx ptr

  | InstrID.ID_Add
  | InstrID.ID_FAdd
  | InstrID.ID_Sub
  | InstrID.ID_FSub
  | InstrID.ID_Mul
  | InstrID.ID_FMul
  | InstrID.ID_UDiv
  | InstrID.ID_SDiv
  | InstrID.ID_FDiv
  | InstrID.ID_URem
  | InstrID.ID_SRem
  | InstrID.ID_FRem ->
    readInstrBinary (InstrID.toBinOpType id |> AST.mkBinOp ptr) ctx ptr
  | InstrID.ID_ICmp ->
    readInstrBinary (readICmpType ptr |> AST.mkICmp ptr) ctx ptr
  | InstrID.ID_FCmp ->
    readInstrBinary (readFCmpType ptr |> AST.mkFCmp ptr) ctx ptr

  | InstrID.ID_Shl
  | InstrID.ID_LShr
  | InstrID.ID_AShr
  | InstrID.ID_And
  | InstrID.ID_Or
  | InstrID.ID_Xor ->
    readInstrBinary (InstrID.toLogicOpType id |> AST.mkLogicOp ptr) ctx ptr

  | InstrID.ID_Alloca -> readAlloca ctx ptr
  | InstrID.ID_Load -> readLoad ctx ptr
  | InstrID.ID_Store -> readStore ctx ptr
  | InstrID.ID_GetElementPtr ->
    readGetElemPtr readOperand (AST.mkGetElemPtr ptr) ctx ptr
  | InstrID.ID_Trunc
  | InstrID.ID_ZExt
  | InstrID.ID_SExt
  | InstrID.ID_FPToUI
  | InstrID.ID_FPToSI
  | InstrID.ID_UIToFP
  | InstrID.ID_SIToFP
  | InstrID.ID_FPTrunc
  | InstrID.ID_FPExt
  | InstrID.ID_PtrToInt
  | InstrID.ID_IntToPtr
  | InstrID.ID_BitCast
  | InstrID.ID_AddrSpaceCast -> readCast readOperand (AST.mkCast ptr) ctx id ptr

  | InstrID.ID_PHI -> readPhi ctx ptr
  | InstrID.ID_Call -> readCall ctx ptr
  | InstrID.ID_Select -> readTriple readOperand (AST.mkSelect ptr) ctx ptr
  | InstrID.ID_UserOp1
  | InstrID.ID_UserOp2
  | InstrID.ID_VAArg
  | InstrID.ID_ExtractElement ->
    readBinary readOperand (AST.mkExtractElem ptr) ctx ptr
  | InstrID.ID_InsertElement ->
    readTriple readOperand (AST.mkInsertElem ptr) ctx ptr
  | InstrID.ID_ShuffleVector -> readShuffle ctx ptr
  | InstrID.ID_ExtractValue ->
    readExtractVal readOperand (AST.mkExtractVal ptr) ctx ptr
  | InstrID.ID_InsertValue ->
    readInsertVal readOperand (AST.mkUndefT >> Operand.Constant)
                  (AST.mkInsertVal ptr) ctx ptr
  | InstrID.ID_Fence -> ctx, readAtomicity getFence ptr |> AST.mkFence ptr
  | InstrID.ID_AtomicCmpXchg -> readCmpXchg ctx ptr
  | InstrID.ID_AtomicRMW -> readAtmoicRMW ctx ptr
  | InstrID.ID_Resume -> readUnOp (AST.mkResume ptr) ctx ptr
  | InstrID.ID_LandingPad -> readLandingPad ctx ptr
  | InstrID.ID_CleanupRet
  | InstrID.ID_CatchRet
  | InstrID.ID_CatchPad
  | InstrID.ID_CleanupPad
  | InstrID.ID_CatchSwitch -> dump ptr; failwithf "todo %A" id
  | _ -> ReaderException "readInstr fail" |> raise
  |> addName

let readBlock ctx ptr =
  let name = getValName ctx ptr
  let ctx, instrs =
    getPtrArray getInstrCnt getInstrs ptr |> Array.mapState ctx readInstr
  ctx, (name, instrs)

let readBlocks ctx ptr =
  getPtrArray getBlockCnt getBlocks ptr |> Array.mapState ctx readBlock

let readFunc ctx ptr =
  let ctx = Context.setFuncPtr ctx ptr
  let id = getValName ctx ptr, ptr
  let ctx, funcTy = getValueType ptr |> getFuncTy ctx
  let ctx, params_ = readParams ctx ptr
  let ctx, blocks = readBlocks ctx ptr
  let isDeclr = isDeclr ptr
  let ctx, md = getFuncMD ptr |> loadMetadata ctx
  ctx,
  AST.mkFunc ptr id funcTy (params_, Tuple.third funcTy) blocks isDeclr md

let readGlobal ctx ptr =
  let id = getValName ctx ptr, ptr
  let ctx, ty = getValType ctx ptr
  let ctx, consT = getGlobal ptr |> readConstantOpt ctx
  let ctx, md = getGlobalMD ptr |> loadMetadata ctx
  ctx, AST.mkGlobal id ty consT (isWritable ptr) md

let readModule ctx ptr =
  let ctx = Context.init ctx ptr
  let ctx, globals =
    getPtrArray getGlobalSize getGlobals ptr |> Array.mapState ctx readGlobal
  let ctx, funcs =
    getPtrArray getFuncCnt getFuncs ptr |> Array.mapState ctx readFunc
  AST.mkModule ptr (getModuleName ptr) globals funcs (Context.getTypes ctx)
               (Context.getMetadatas ctx)

let asyncReadModule ctx ptr = async {
  return readModule ctx ptr
}

let readOne fname =
  Logger.info "Start parsing %s" fname
  let llvm = getContext()
  let ptr = parseBitCode (llvm, fname)
  Logger.info "Start loading %s" fname
  readModule Context.empty ptr

let readAll fnames =
  let llvm = getContext()
  let parse fname = parseBitCode (llvm, fname)
  Array.map parse fnames |> Utils.doParallel (asyncReadModule Context.empty)
