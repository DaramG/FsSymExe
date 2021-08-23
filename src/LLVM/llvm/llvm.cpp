#include <llvm/IR/Module.h>
#include <llvm/Demangle/Demangle.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ModuleSlotTracker.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm-c/Core.h>

using namespace llvm;

enum FSharpInstrID {
	FSharpRet            = 0,
	FSharpBr             = 1,
	FSharpSwitch         = 2,
	FSharpIndirectBr     = 3,
	FSharpInvoke         = 4,
	FSharpUnreachable    = 5,
	FSharpCallBr         = 6,
	FSharpFNeg           = 7,
	FSharpAdd            = 8,
	FSharpFAdd           = 9,
	FSharpSub            = 10,
	FSharpFSub           = 11,
	FSharpMul            = 12,
	FSharpFMul           = 13,
	FSharpUDiv           = 14,
	FSharpSDiv           = 15,
	FSharpFDiv           = 16,
	FSharpURem           = 17,
	FSharpSRem           = 18,
	FSharpFRem           = 19,
	FSharpShl            = 20,
	FSharpLShr           = 21,
	FSharpAShr           = 22,
	FSharpAnd            = 23,
	FSharpOr             = 24,
	FSharpXor            = 25,
	FSharpAlloca         = 26,
	FSharpLoad           = 27,
	FSharpStore          = 28,
	FSharpGetElementPtr  = 29,
	FSharpTrunc          = 30,
	FSharpZExt           = 31,
	FSharpSExt           = 32,
	FSharpFPToUI         = 33,
	FSharpFPToSI         = 34,
	FSharpUIToFP         = 35,
	FSharpSIToFP         = 36,
	FSharpFPTrunc        = 37,
	FSharpFPExt          = 38,
	FSharpPtrToInt       = 39,
	FSharpIntToPtr       = 40,
	FSharpBitCast        = 41,
	FSharpAddrSpaceCast  = 42,
	FSharpICmp           = 43,
	FSharpFCmp           = 44,
	FSharpPHI            = 45,
	FSharpCall           = 46,
	FSharpSelect         = 47,
	FSharpUserOp1        = 48,
	FSharpUserOp2        = 49,
	FSharpVAArg          = 50,
	FSharpExtractElement = 51,
	FSharpInsertElement  = 52,
	FSharpShuffleVector  = 53,
	FSharpExtractValue   = 54,
	FSharpInsertValue    = 55,
	FSharpFence          = 56,
	FSharpAtomicCmpXchg  = 57,
	FSharpAtomicRMW      = 58,
	FSharpResume         = 59,
	FSharpLandingPad     = 60,
	FSharpCleanupRet     = 61,
	FSharpCatchRet       = 62,
	FSharpCatchPad       = 63,
	FSharpCleanupPad     = 64,
  FSharpCatchSwitch    = 65,
  FSharpFreeze    = 66,
};

extern "C" void dump (Value *value){
  value->dump();
}

extern "C" LLVMContext *getContext() {
  return new LLVMContext();
}

extern "C" const char* ptrToString (void *ptr) {
  if (ptr) return (const char *) ptr;
  else return strdup ("");
}

extern "C" Module *parseBitCode (LLVMContext *context, const char *fname){
  SMDiagnostic err;
  std::unique_ptr<Module> ret = parseIRFile (fname, err, *context);
  if (ret == NULL) {
    printf("Invalid BitCode: %s\n", fname);
    exit(-1);
  }
  return ret.release();
}

extern "C" const char* getModuleName (Module *module) {
  return strdup(module->getSourceFileName().c_str());
}

extern "C" int getFuncCnt (Module *module) {
  return module->size();
}

extern "C" void getFuncs (Module *module, Function** ret) {
  for (auto &f: *module){
    *ret++ = &f;
  }
}

extern "C" int getBlockCnt (Function *func) {
  return func->size();
}

extern "C" void getBlocks (Function *func, BasicBlock** ret) {
  for (auto &bb: *func){
    *ret++ = &bb;
  }
}

extern "C" int getParamCnt (Function *func) {
  return func->arg_size();
}

extern "C" void getParams (Function *func, Argument** ret) {
  for (auto &arg: func->args()){
    *ret++ = &arg;
  }
}

extern "C" int getParamTyCnt (FunctionType *func) {
  return func->getNumParams();
}

extern "C" void getParamTys (FunctionType *func, Type** ret) {
  for (auto &param: func->params()){
    *ret++ = param;
  }
}

extern "C" int getStructElemCnt (StructType *ty) {
  return ty->getNumElements();
}

extern "C" void getStructElems (Module *module, StructType *ty, Type** ret, uint64_t* idxs) {
  if (ty->isSized()) {
    auto layout = module->getDataLayout().getStructLayout(ty);
    unsigned i = 0;
    for (auto &elem: ty->elements()){
      *ret++ = elem;
      *idxs++ = layout->getElementOffsetInBits(i);
      i ++;
    }
  }
}

extern "C" int getInstrCnt (BasicBlock *bb) {
  return bb->size();
}

extern "C" void getInstrs (BasicBlock *bb, Instruction** ret) {
  for (auto &inst: *bb){
    *ret++ = &inst;
  }
}

extern "C" Type *getValueType (Value *value){
  return value->getType();
}

extern "C" Type *getElemType (Type *ty){
  if (auto *pTy = dyn_cast<PointerType>(ty)) return pTy->getElementType();
  if (auto *aTy = dyn_cast<ArrayType>(ty)) return aTy->getElementType();
  return (cast<VectorType>(ty))->getElementType();
}

extern "C" Type *getRetType (FunctionType *ty){
  return ty->getReturnType();
}

extern "C" bool hasVarArg (FunctionType *ty) {
  return ty->isVarArg();
}

extern "C" bool isPacked (StructType *ty) {
  return ty->isPacked();
}

extern "C" unsigned getArgNo (Argument *arg) {
  return arg->getArgNo();
}

extern "C" unsigned getPtrAddrSpc (PointerType *ty) {
  return ty->getAddressSpace();
}

extern "C" unsigned getIntTypeWidth (IntegerType *ty) {
  return ty->getBitWidth();
}

extern "C" unsigned getVecSize (VectorType *ty) {
  return ty->getNumElements();
}

extern "C" unsigned getArrLen (ArrayType *ty) {
  return ty->getNumElements();
}

extern "C" ModuleSlotTracker *getTracker (Module *module) {
  ModuleSlotTracker *tracker = new ModuleSlotTracker(module);
  return tracker;
}

extern "C" void setTrackerFunc (ModuleSlotTracker *tracker, Function *func) {
  tracker->incorporateFunction(*func);
}

extern "C" const char* demangle (const char *ty){
  std::string str(ty);
  return strdup (demangle(str).data());
}

extern "C" const char* getValueName (ModuleSlotTracker *tracker, Value* value){
  if (value->hasName()) {
    return strdup(value->getName().data());
  }

  if(tracker->getCurrentFunction() == NULL){
     if (const BasicBlock *bb = dyn_cast<BasicBlock>(value))
       tracker->incorporateFunction(*(bb->getParent()));
  }

  int idx = tracker->getLocalSlot(value);
  if (idx < 0) return strdup("");
  else return strdup(std::to_string(idx).data());
}

extern "C" const char* getStructName (StructType* ty) {
  if(ty->hasName()) return strdup(ty->getName().data());
  return strdup("");
}

extern "C" unsigned getTypeID (Type *ty) {
  switch (ty->getTypeID()) {
    case Type::VoidTyID:      return 0;
    case Type::HalfTyID:      return 1;
    case Type::FloatTyID:     return 2;
    case Type::DoubleTyID:    return 3;
    case Type::X86_FP80TyID:  return 4;
    case Type::FP128TyID:     return 5;
    case Type::PPC_FP128TyID: return 6;
    case Type::LabelTyID:     return 7;
    case Type::IntegerTyID:   return 8;
    case Type::FunctionTyID:  return 9;
    case Type::StructTyID:    return 10;
    case Type::ArrayTyID:     return 11;
    case Type::PointerTyID:   return 12;
    case Type::FixedVectorTyID:
    case Type::ScalableVectorTyID:    return 13;
    case Type::MetadataTyID:  return 14;
    case Type::X86_MMXTyID:   return 15;
    case Type::TokenTyID:     return 16;
    default:
      puts ("getTypeID fail");
      exit (-1);
  }
}

static FSharpInstrID toInstrID (int opcode)
{
  switch (opcode) {
    default: llvm_unreachable("Unhandled Opcode.");
#define HANDLE_INST(num, opc, clas) case num: return FSharp##opc;
#include <llvm/IR/Instruction.def>
#undef HANDLE_INST
  }
}

extern "C" uint32_t getInstrID (Instruction *instr) {
  return toInstrID (instr->getOpcode());
}

extern "C" unsigned getOperandID (Value *value){
  if(isa<Argument>(value)) return 0;
  if(isa<Instruction>(value)) return 1;
  if(isa<Constant>(value)) return 2;
  if(isa<MetadataAsValue>(value)) return 3;
  puts ("getOperandID fail");
  exit(-1);
}

extern "C" Metadata *getMetadata (MetadataAsValue *value){
  return value->getMetadata ();
}

extern "C" void getMetadataLocalVar (const Metadata *md, void **ret) {
  const DILocalVariable *v = dyn_cast<DILocalVariable>(md);
  const char *name = v->getName().data();
  if (name) ret[0] = strdup (name);
  ret[1] = v->getRawType();
}

extern "C" Value *getMetadataLocalAs (LocalAsMetadata *value){
  return value->getValue ();
}

extern "C" unsigned getMetadataID (const Metadata *md){
  if (md == NULL) return 7;
  if(isa<DILocalVariable>(md)) return 0; 
  if(isa<DIDerivedType>(md)) return 1; 
  if(isa<DICompositeType>(md)) return 2; 
  if(isa<LocalAsMetadata>(md)) return 3; 
  if(isa<DISubprogram>(md)) return 4; 
  if(isa<DISubroutineType>(md)) return 5; 
  if(isa<DIBasicType>(md)) return 6; 
  return 8;
}

extern "C" unsigned getMDDerivedTypeTag (const DIDerivedType *md){
  switch (md->getTag()) {
    case dwarf::DW_TAG_member: return 0;
    case dwarf::DW_TAG_typedef: return 1;
    case dwarf::DW_TAG_inheritance: return 2;
    case dwarf::DW_TAG_friend: return 3;
    case dwarf::DW_TAG_pointer_type: return 4;
    case dwarf::DW_TAG_reference_type: return 5;
    case dwarf::DW_TAG_ptr_to_member_type: return 6;
    case dwarf::DW_TAG_const_type: return 7;
    case dwarf::DW_TAG_volatile_type: return 8;
    case dwarf::DW_TAG_restrict_type: return 9;
    case dwarf::DW_TAG_atomic_type: return 10;
    case dwarf::DW_TAG_rvalue_reference_type: return 11;
    default:
      printf ("getMDDerivedTypeTag fail: %d\n", md->getTag());
      exit (-1);
  }
}

extern "C" const char *getMDDerivedTypeName (const DIDerivedType *md){
  const char * name = md->getName().data();
  if (name) return strdup (name);
  else return strdup ("");
}

extern "C" void getMDDerivedType (const DIDerivedType *md, uint64_t *ret) {
  *ret++ = getMDDerivedTypeTag (md);
  *ret++ = (uint64_t) getMDDerivedTypeName (md);
  *ret++ = (uint64_t) md->getRawBaseType ();
  *ret++ = md->getOffsetInBits();
}

extern "C" unsigned getMDCompositeTypeTag (const DICompositeType *md){
  switch (md->getTag()) {
    case dwarf::DW_TAG_class_type: return 0;
    case dwarf::DW_TAG_structure_type: return 1;
    case dwarf::DW_TAG_array_type: return 2;
    case dwarf::DW_TAG_union_type: return 3;
    case dwarf::DW_TAG_enumeration_type: return 4;
    default:
      printf ("getMDCompositeTypeTag fail: %d\n", md->getTag());
      exit (-1);
  }
}

const char* getCStr (const char* ret) {
  if (ret) return strdup (ret);
  else return strdup ("");
}

extern "C" const char *getMDCompositeTypeName (const DICompositeType *md){
  return getCStr (md->getName().data());
}

extern "C" int getMDArrayCnt (const MDTuple *arr){
  if(arr) return arr->getNumOperands();
  else return 0;
}

extern "C" void getMDArray (const MDTuple *arr, const Metadata **ret){
  if (!arr) return;
  for (unsigned int i = 0; i < arr->getNumOperands(); i++){
    *ret ++ = arr->getOperand(i);
  }
}

extern "C" Metadata *getMDSubRoutine (const DISubroutineType *md) {
  return md->getRawTypeArray ();
}

extern "C" void getMDCompositeType (const DICompositeType *md, uint64_t *ret) {
  *ret++ = getMDCompositeTypeTag (md);
  *ret++ = (uint64_t) getCStr (md->getName().data());
  *ret++ = (uint64_t) getCStr (md->getIdentifier().data());
  *ret++ = (uint64_t) md->getRawTemplateParams();
  *ret++ = (uint64_t) md->getRawElements();
  *ret++ = md->getOffsetInBits();
}

extern "C" void getMDSubProgram (const DISubprogram *md, uint64_t *ret) {
  *ret++ = (uint64_t) getCStr (md->getName().data());
  *ret++ = (uint64_t) getCStr (md->getLinkageName().data());
  *ret++ = (uint64_t) md->getType();
  if (md->getVirtuality() != dwarf::DW_VIRTUALITY_none ||
      md->getVirtualIndex() != 0)
    *ret++ = md->getVirtualIndex();
  else *ret++ = -1;
}

extern "C" const char* getMDBasicTypeName (const DIBasicType *md) {
  return getCStr (md->getName().data());
}

extern "C" int getMDBasicTypeSize (const DIBasicType *md) {
  return md->getSizeInBits();
}

extern "C" Metadata *getFuncMD (Function *func) {
  return func->getSubprogram ();
}

extern "C" Metadata *getGlobalMD (GlobalVariable *global) {
  const Metadata *md = global->getMetadata(LLVMContext::MD_dbg);
  if (!md) return NULL;

  auto *m1 = dyn_cast<DIGlobalVariableExpression>(md);
  if(!m1) return NULL;
  
  auto *var = m1->getVariable();
  if(!var) return NULL;

  auto *m2 = dyn_cast<DIGlobalVariable>(var);
  if(!m2) return NULL;

  return m2->getRawType();
}

extern "C" unsigned getConstantID (Constant *cons){
  switch(cons->getValueID()){
    case Value::ConstantExprVal:
      return toInstrID (dyn_cast<ConstantExpr>(cons)->getOpcode());
    case Value::ConstantIntVal: return 0x100;
    case Value::ConstantPointerNullVal: return 0x107;
    case Value::ConstantAggregateZeroVal: return 0x108;
    case Value::ConstantStructVal: return 0x109;
    case Value::ConstantArrayVal:
    case Value::ConstantDataArrayVal:
      return 0x10a;
    case Value::ConstantVectorVal:
    case Value::ConstantDataVectorVal:
      return 0x10b;
    case Value::UndefValueVal: return 0x10c;
    case Value::BlockAddressVal: return 0x10d;
    case Value::GlobalVariableVal:
    case Value::GlobalAliasVal:
    case Value::FunctionVal:
      return 0x10e;
    case Value::ConstantTokenNoneVal: return 0x10f;
    case Value::ConstantFPVal: {
      const APFloat &APF = dyn_cast<ConstantFP>(cons)->getValueAPF();
      auto semantics = &APF.getSemantics();
      if(semantics == &APFloat::IEEEhalf()) return 0x101;
      if(semantics == &APFloat::IEEEsingle()) return 0x102;
      if(semantics == &APFloat::IEEEdouble()) return 0x103;
      if(semantics == &APFloat::IEEEquad()) return 0x104;
      if(semantics == &APFloat::x87DoubleExtended()) return 0x105;
      if(semantics == &APFloat::PPCDoubleDouble()) return 0x106;
      printf ("getConstantID float fail\n");
      break;
    }
    default:
      printf ("getConstantID fail: %d\n", cons->getValueID());
  }
  cons->dump();
  exit(-1);
}

extern "C" Value *getRet (ReturnInst *ret) {
  return ret->getReturnValue();
}

extern "C" void getBr (BranchInst *br, void** ret) {
  if (br->isConditional()){
    ret[0] = br->getCondition();
    ret[2] = br->getSuccessor(1);
  }
  ret[1] = br->getSuccessor(0);
}
extern "C" void getAlloca (Module *module, AllocaInst *alloca, void** ret) {
  auto layout = module->getDataLayout();
  auto ty = alloca->getAllocatedType ();
  ret[0] = ty;
  ret[1] = alloca->getArraySize();
  ret[2] = (void *)(uint64_t)layout.getTypeAllocSizeInBits(ty);
}

extern "C" void getStore (StoreInst *store, void** ret) {
  ret[0] = store->getPointerOperand();
  ret[1] = store->getValueOperand();
  ret[2] = (void *)(uint64_t)store->getAlignment();
}

extern "C" void getLoad (Module *module, LoadInst *load, void** ret) {
  auto layout = module->getDataLayout();
  ret[0] = load->getPointerOperand();
  ret[1] = (void *)(uint64_t)layout.getTypeSizeInBits(load->getType());
}

extern "C" int getConstantIntSize (ConstantInt *cons) {
  return cons->getBitWidth();
}
extern "C" void getConstantInt (ConstantInt *cons, char* ret) {
  memcpy (ret, cons->getValue().getRawData(), (cons->getBitWidth() + 7) /8);
}

extern "C" Value *getUnOp (UnaryOperator *inst) {
  return inst->getOperand(0);
}

extern "C" void getBinOp (BinaryOperator *bin, void** ret) {
  ret[0] = bin->getOperand(0);
  ret[1] = bin->getOperand(1);
}

extern "C" void getTriple (Instruction *ins, void** ret) {
  ret[0] = ins->getOperand(0);
  ret[1] = ins->getOperand(1);
  ret[2] = ins->getOperand(2);
}

extern "C" void getShuffle (Instruction *ins, void** ret) {
  ret[0] = ins->getOperand(0);
  ret[1] = ins->getOperand(1);
  ret[2] = (cast<ShuffleVectorInst>(ins))->getShuffleMaskForBitcode();
}

extern "C" void getCast (CastInst *cast, void** ret) {
  ret[0] = cast->getDestTy();
  ret[1] = cast->getOperand(0);
}

extern "C" int getCallSize (CallInst *call) {
  return call->arg_size() + 1;
}

extern "C" void getCall (CallInst *call, void** ret) {
  *ret ++ = call->getCalledOperand ();
  for (auto &arg : call->args()){
    *ret ++ = &*arg;
  }
}

extern "C" int getGetElemPtrSize (GetElementPtrInst *inst) {
  return inst->getNumIndices() + 2;
}

extern "C" void getGetElemPtr (GetElementPtrInst *inst, void** ret) {
  *ret ++ = (void *) inst->isInBounds();
  for (auto &op : inst->operands()){
    *ret ++ = &*op;
  }
}

extern "C" Instruction *consToInstr (ConstantExpr *cons){
  return cons->getAsInstruction();
}

extern "C" int getSwitchSize (SwitchInst *inst) {
  return inst->getNumCases() + 1;
}

extern "C" void getSwitch (SwitchInst *inst, void **ret, BasicBlock **bb) {
  *ret++ = inst->getCondition();
  *bb++ = inst->getDefaultDest();
  for (auto &_case : inst->cases()){
    *ret++ = _case.getCaseValue();
    *bb++ = _case.getCaseSuccessor();
  }
}

extern "C" int getPhiSize (PHINode *node) {
  return node->getNumIncomingValues() +1;
}

extern "C" void getPhi (PHINode *node, void **ret, BasicBlock **bb) {
  *ret ++ = node->getType ();
  *bb ++ = NULL;
  for (unsigned i = 0, e = node->getNumIncomingValues(); i < e; i++){
    *ret++ = node->getIncomingValue(i);
    *bb++ = node->getIncomingBlock(i);
  }
}

extern "C" int getConstantArrSize (ConstantDataSequential *cons) {
  if (const ConstantArray *CA = dyn_cast<ConstantArray>(cons)){
    return CA->getNumOperands();
  }
  if (const ConstantDataArray *CA = dyn_cast<ConstantDataArray>(cons)){
    return CA->getNumElements();
  }
  assert(false);
}

extern "C" void getConstantArr (ConstantDataSequential *cons, Constant **ret) {
  if (const ConstantArray *CA = dyn_cast<ConstantArray>(cons)){
    int num = CA->getNumOperands();
    for ( int  i = 0; i < num; i++){
      ret[i] = CA->getOperand(i);
    }
    return;
  }
  if (const ConstantDataArray *CA = dyn_cast<ConstantDataArray>(cons)){
    int num = CA->getNumElements();
    for ( int  i = 0; i < num; i++){
      ret[i] = CA->getElementAsConstant(i);
    }
    return;
  }
  assert(false);
}

extern "C" bool isConstantStr (ConstantDataArray *cons) {
  return cons->isString ();
}

extern "C" int getConstantStrSize (ConstantDataArray *cons) {
  return cons->getNumElements();
}

extern "C" const char* getConstantStr (ConstantDataArray *cons, char *buf, int size) {
  const char *src = cons->getAsString().data();
  memcpy (buf, src, size);
  return src;
}

extern "C" int getConstantVecSize (ConstantVector *cons) {
  return cast<VectorType>(cons->getType())->getNumElements();
}

extern "C" void getConstantVec (ConstantVector *cons, Constant **ret) {
  int num = getConstantVecSize(cons);
  for ( int  i = 0; i < num; i++){
    ret[i] = dyn_cast<Constant>(cons->getAggregateElement(i));
  }
}

extern "C" int getConstantStructSize (ConstantStruct *cons) {
  return cons->getNumOperands();
}

extern "C" void getConstantStruct (ConstantStruct *cons, Constant **ret) {
  int num = cons->getNumOperands ();
  for ( int  i = 0; i < num; i++){
    ret[i] = cons->getOperand(i);
  }
}

extern "C" int getGlobalSize (Module *module) {
  return module->getGlobalList().size();
}

extern "C" void getGlobals (Module *module, GlobalVariable **ret) {
  for (auto &var : module->globals()){
    *ret++ = &var;
  }
}

extern "C" Constant *getGlobal (GlobalVariable *global) {
  if(global->hasInitializer()) return global->getInitializer();
  return NULL;
}

extern "C" bool isWritable (GlobalVariable *global) {
  return not global->isConstant();
}

extern "C" bool isInlineAsm (Value *value) {
  return isa<InlineAsm>(value);
}

extern "C" void getBytes (Constant *cons, char *ret, int size){
  char *buf = (char *)dyn_cast<ConstantFP>(cons)->getValueAPF().bitcastToAPInt().getRawData();
  memcpy (ret, buf, size);
}

extern "C" int getIndiceSize (ExtractValueInst *inst) {
  return inst->getNumIndices();
}

extern "C" void getIndices (ExtractValueInst *inst, void**ret) {
  ArrayRef<unsigned> indices = inst->getIndices();
  for (int  i = 0, e = inst->getNumIndices(); i < e; i++){
    *ret ++ = (void *)(uint64_t)indices[i];
  }
}

extern "C" Value *getExtract (ExtractValueInst *inst) {
  return inst->getOperand(0);
}

extern "C" void getInsert (InsertValueInst *inst, Value **ret) {
  ret[0] = inst->getOperand(0);
  ret[1] = inst->getOperand(1);
}

extern "C" const char* getInlineAsmCode (InlineAsm *inst) {
  return strdup(inst->getAsmString().data());
}

extern "C" const char* getInlineAsmConstraint (InlineAsm *inst) {
  return strdup(inst->getConstraintString().data());
}

extern "C" void getInvokeDsts (InvokeInst *inst, void **ret){
  ret[0] = inst->getNormalDest();
  ret[1] = inst->getUnwindDest();
}

extern "C" int getLandingPadSize (LandingPadInst *pad) {
  return pad->getNumClauses();
}

extern "C" void getLandingPad (LandingPadInst *pad, Constant **catches, Constant** filters) {
  for(int i = 0, e = pad->getNumClauses(); i < e; i++){
    if(pad->isCatch(i)) *catches++ = pad->getClause(i);
    else *filters ++ = pad->getClause(i);
  }
}
extern "C" bool isCleanUp (LandingPadInst *pad) {
  return pad->isCleanup();
}

extern "C" void getBlockAddr (BlockAddress *ba, Value **ret){
  ret[0] = ba->getFunction();
  ret[1] = ba->getBasicBlock();
}

extern "C" int getOperandSize (Instruction *inst) {
  return inst->getNumOperands ();
}

extern "C" void getOperands (Instruction *inst, Value **ret) {
  for (auto &op: inst->operands()){
    *ret ++ = &*op;
  }
}

uint64_t toSyncScopeID (SyncScope::ID id){
  switch (id) {
    case SyncScope::SingleThread: return 0;
    case SyncScope::System: return 1;
  }
  printf("Invalid SyncScopeID: %d\n", id);
  exit(-1);
}

uint64_t toMemoryOrderID (AtomicOrdering id) {
  switch(id) {
    case AtomicOrdering::NotAtomic: return 0;
    case AtomicOrdering::Unordered: return 1;
    case AtomicOrdering::Monotonic: return 2;
    case AtomicOrdering::Acquire: return 3;
    case AtomicOrdering::Release: return 4;
    case AtomicOrdering::AcquireRelease: return 5;
    case AtomicOrdering::SequentiallyConsistent: return 6;
  }
  printf("Invalid MemoryOrderID: %d\n", (int) id);
  exit(-1);
}

extern "C" void getFence (FenceInst *inst, void **ret) {
  *ret ++ = (void*)toSyncScopeID(inst->getSyncScopeID());
  *ret ++ = (void*)toMemoryOrderID(inst->getOrdering());
}

extern "C" void getCmpXchAtomicity (AtomicCmpXchgInst *inst, void **ret){
  *ret ++ = (void*)toSyncScopeID(inst->getSyncScopeID());
  *ret ++ = (void*)toMemoryOrderID(inst->getSuccessOrdering());
  *ret ++ = (void*)toMemoryOrderID(inst->getFailureOrdering());
}

extern "C" void getRMWAtomicity (AtomicRMWInst *inst, void **ret) {
  *ret ++ = (void*)toSyncScopeID(inst->getSyncScopeID());
  *ret ++ = (void*)toMemoryOrderID(inst->getOrdering());
}

extern "C" uint64_t getRMWOp (AtomicRMWInst *inst){
  switch(inst->getOperation()){
    case AtomicRMWInst::Xchg: return 0;
    case AtomicRMWInst::Add: return 1;
    case AtomicRMWInst::Sub: return 2;
    case AtomicRMWInst::And: return 3;
    case AtomicRMWInst::Nand: return 4;
    case AtomicRMWInst::Or: return 5;
    case AtomicRMWInst::Xor: return 6;
    case AtomicRMWInst::Max: return 7;
    case AtomicRMWInst::Min: return 8;
    case AtomicRMWInst::UMax: return 9;
    case AtomicRMWInst::UMin: return 10;
    case AtomicRMWInst::FAdd: return 11;
    case AtomicRMWInst::FSub: return 12;
  }
  printf("Invalid RMWOp\n");
  exit(-1);
}

extern "C" void getStructAllocSize (Module *module, StructType *ty, uint64_t *ret){
  if (ty->isSized()) {
    auto layout = module->getDataLayout();
    ret[0] = layout.getTypeAllocSizeInBits(ty);
    ret[1] = layout.getStructLayout(ty)->getAlignment().value();
  }
}

uint64_t toCmpID (unsigned op){
  switch (op) {
    case ICmpInst::Predicate::FCMP_FALSE: return 0;
    case ICmpInst::Predicate::FCMP_OEQ:   return 1;
    case ICmpInst::Predicate::FCMP_OGT:   return 2;
    case ICmpInst::Predicate::FCMP_OGE:   return 3;
    case ICmpInst::Predicate::FCMP_OLT:   return 4;
    case ICmpInst::Predicate::FCMP_OLE:   return 5;
    case ICmpInst::Predicate::FCMP_ONE:   return 6;
    case ICmpInst::Predicate::FCMP_ORD:   return 7;
    case ICmpInst::Predicate::FCMP_UNO:   return 8;
    case ICmpInst::Predicate::FCMP_UEQ:   return 9;
    case ICmpInst::Predicate::FCMP_UGT:   return 10;
    case ICmpInst::Predicate::FCMP_UGE:   return 11;
    case ICmpInst::Predicate::FCMP_ULT:   return 12;
    case ICmpInst::Predicate::FCMP_ULE:   return 13;
    case ICmpInst::Predicate::FCMP_UNE:   return 14;
    case ICmpInst::Predicate::FCMP_TRUE:  return 15;

    case ICmpInst::Predicate::ICMP_EQ:    return 16;
    case ICmpInst::Predicate::ICMP_NE:    return 17;
    case ICmpInst::Predicate::ICMP_UGT:   return 18;
    case ICmpInst::Predicate::ICMP_UGE:   return 19;
    case ICmpInst::Predicate::ICMP_ULT:   return 20;
    case ICmpInst::Predicate::ICMP_ULE:   return 21;
    case ICmpInst::Predicate::ICMP_SGT:   return 22;
    case ICmpInst::Predicate::ICMP_SGE:   return 23;
    case ICmpInst::Predicate::ICMP_SLT:   return 24;
    case ICmpInst::Predicate::ICMP_SLE:   return 25;
  }
  printf("Invalid CmpID\n");
  exit(-1);
}

extern "C" uint64_t getConstantPred (ConstantExpr *cons){
  return toCmpID (cons->getPredicate ());
}

extern "C" uint64_t getInstrPred (CmpInst *instr){
  return toCmpID (instr->getPredicate ());
}

extern "C" bool isDeclr (Function *func){
  return func->isDeclaration();
}
