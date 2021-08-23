namespace LLVM

open LLVM.Core
open Common

type VisitFunc<'ctx, 'ty> = 'ctx -> 'ty -> 'ctx * bool
type VisitIdxFunc<'ctx, 'ty> = 'ctx -> int -> 'ty -> 'ctx * bool

type Visitor<'ctx> = {
  Type: VisitFunc<'ctx, Type>
  ConstantT: VisitFunc<'ctx, ConstantT>
  Function: VisitFunc<'ctx, Function>
  BasicBlock: VisitIdxFunc<'ctx, BasicBlock>
  Instruction: VisitIdxFunc<'ctx, Instruction>
}

module Visitor =
  let rec visitType visitor ty ctx =
    let ctx, isEnd = visitor.Type ctx ty
    if isEnd then ctx
    else
      match ty with
      | Void _
      | Integer _
      | Type.Float _
      | X86Mmx _
      | Label _
      | Type.Metadata _
      | Token _
      | Named _ -> ctx
      | Function (funcTy, _) -> visitFunctionTy visitor funcTy ctx
      | Type.Struct (structTy, _) -> visitStructTy visitor structTy ctx
      | Type.Array (ty, _, _)
      | Type.Pointer (ty, _, _)
      | Type.Vector (ty, _, _) -> visitType visitor ty ctx

  and visitStructTy visitor (_, tys, _, _, _) ctx =
    Array.foldRev (fst >> visitType visitor) tys ctx

  and visitFunctionTy visitor (retTy, paramTys, _) ctx =
    visitType visitor retTy ctx |> Array.foldRev (visitType visitor) paramTys

  let rec visitConstant visitor cons ctx =
    match cons with
    | Int _
    | Float _
    | BlockAddr _
    | TokenNone -> ctx
    | Null ty
    | AggregateZero ty
    | Undef ty
    | Global (ty, _) -> visitType visitor ty ctx
    | Struct (ty, conss) -> visitStruct visitor ty conss ctx
    | Constant.Array (ty, conss) -> visitArray visitor ty conss ctx
    | Vector conss -> visitConstantTs visitor conss ctx
    | Constant.BinOp (_, c1, c2)
    | Constant.ICmp (_, c1, c2)
    | Constant.FCmp (_, c1, c2)
    | Constant.LogicOp (_, c1, c2)
    | Constant.ExtractElem (c1, c2)
    | Constant.InsertVal (c1, _, c2) -> visitConstantT2 visitor c1 c2 ctx
    | Constant.Select (c1, c2, c3)
    | Constant.InsertElem (c1, c2, c3)
    | Constant.Shuffle (c1, c2, c3) -> visitConstantT3 visitor c1 c2 c3 ctx
    | Constant.ExtractVal (cons, _) -> visitConstantT visitor cons ctx
    | Constant.Cast (_, cons, ty) -> visitConstantType visitor cons ty ctx
    | Constant.GetElemPtr (_, c1, cs) -> visitConstantTs2 visitor c1 cs ctx

  and visitConstantT2 visitor c1 c2 ctx =
    visitConstantT visitor c1 ctx |> visitConstantT visitor c2

  and visitConstantT3 visitor c1 c2 c3 ctx =
    visitConstantT visitor c1 ctx |> visitConstantT visitor c2
    |> visitConstantT visitor c3

  and visitConstantType visitor cons ty ctx =
    visitConstantT visitor cons ctx |> visitType visitor ty

  and visitConstantTs visitor conss ctx =
    Array.foldRev (visitConstantT visitor) conss ctx

  and visitConstantTs2 visitor c1 cs ctx =
    visitConstantT visitor c1 ctx |> visitConstantTs visitor cs

  and visitConstantT visitor (ty, cons, info) ctx =
    let ctx, isEnd = visitor.ConstantT ctx (ty, cons, info)
    if isEnd then ctx
    else visitType visitor ty ctx |> visitConstant visitor cons

  and visitStruct visitor ty conss ctx =
    visitStructTy visitor ty ctx |> visitConstantTs visitor conss

  and visitArray visitor ty conss ctx =
    visitType visitor ty ctx |> visitConstantTs visitor conss

  let visitConstantTOpt visitor consT ctx =
    match consT with
    | Some consT -> visitConstantT visitor consT ctx
    | _ -> ctx

  let visitOperand visitor op ctx =
    match op with
    | Local (ty, _) -> visitType visitor ty ctx
    | Constant cons -> visitConstantT visitor cons ctx
    | Argument (ty, _, _) -> visitType visitor ty ctx
    | _ -> failwith "Not supported yet"

  let visitOperandOpt visitor op ctx =
    match op with
    | Some op -> visitOperand visitor op ctx
    | _ -> ctx

  let visitCallableOperand visitor op ctx =
    match op with
    | CallableOperand.Operand op -> visitOperand visitor op ctx
    | CallableOperand.InlineAsm (ty, _, _) -> visitType visitor ty ctx

  let visitOperand2 visitor o1 o2 ctx =
    visitOperand visitor o1 ctx |> visitOperand visitor o2

  let visitOperand3 visitor o1 o2 o3 ctx =
    visitOperand visitor o1 ctx |> visitOperand visitor o2
    |> visitOperand visitor o3

  let visitOperands visitor ops ctx =
    Array.foldRev (visitOperand visitor) ops ctx

  let visitOperands2 visitor op ops ctx =
    visitOperand visitor op ctx |> visitOperands visitor ops

  let visitTypeOperand visitor ty op ctx =
    visitType visitor ty ctx |> visitOperand visitor op

  let visitSwitch visitor value cases ctx =
    let cons = Array.map fst cases
    visitOperand visitor value ctx
    |> Array.foldRev (visitConstantT visitor) cons

  let visitInvoke visitor callee args ctx =
    visitCallableOperand visitor callee ctx
    |> Array.foldRev (visitOperand visitor) args

  let visitPhi visitor ty ops ctx =
    let folder ctx _ op = visitOperand visitor op ctx
    Map.fold folder (visitType visitor ty ctx) ops

  let visitShuffle visitor o1 o2 cons ctx =
    visitOperand2 visitor o1 o2 ctx |> visitConstantT visitor cons

  let visitLandingPad visitor ty c1 c2 ctx =
    visitType visitor ty ctx
    |> Array.foldRev (visitConstantT visitor) c1
    |> Array.foldRev (visitConstantT visitor) c2

  let visitInstruction visitor idx instr ctx =
    let ctx, isEnd = visitor.Instruction ctx idx instr
    if isEnd then ctx
    else
      match instr with
      | Ret (op, _) -> visitOperandOpt visitor op ctx
      | Switch (value, cases, _, _)  -> visitSwitch visitor value cases ctx
      | CondBr (op, _, _, _)
      | IndirectBr (op, _, _)
      | UnOp (_, op, _)
      | Load (op, _, _)
      | ExtractVal (op, _, _)
      | Resume (op, _)
      | CleanupRet (op, _, _)
      | CatchRet (op, _, _)
      | CatchSwitch (op, _, _, _) -> visitOperand visitor op ctx

      | BinOp (_, o1, o2, _)
      | ICmp (_, o1, o2, _)
      | FCmp (_, o1, o2, _)
      | LogicOp (_, o1, o2, _)
      | Store (o1, o2, _, _)
      | ExtractElem (o1, o2, _)
      | InsertVal (o1, _, o2, _)
      | AtomicRMW (_, o1, o2, _, _) -> visitOperand2 visitor o1 o2 ctx

      | Select (o1, o2, o3, _)
      | InsertElem (o1, o2, o3, _)
      | CmpXchg (o1, o2, o3, _, _, _) -> visitOperand3 visitor o1 o2 o3 ctx

      | Alloca (ty, op, _, _)
      | Cast (_, op, ty, _) -> visitTypeOperand visitor ty op ctx

      | Invoke (callee, args, _, _, _)
      | Call (callee, args, _) -> visitInvoke visitor callee args ctx

      | GetElemPtr (op, ops, _, _)
      | CatchPad (op, ops, _)
      | CleanupPad (op, ops, _) -> visitOperands2 visitor op ops ctx

      | Phi (ty, ops, _) -> visitPhi visitor ty ops ctx
      | Shuffle (o1, o2, cons, _) ->  visitShuffle visitor o1 o2 cons ctx
      | LandingPad (ty, c1, c2, _, _) -> visitLandingPad visitor ty c1 c2 ctx
      | Br _
      | Unreachable _
      | Fence _ -> ctx

  let visitBasicBlock visitor idx (name, instrs) ctx =
    let ctx, isEnd = visitor.BasicBlock ctx idx (name, instrs)
    if isEnd then ctx
    else let instrs = Array.map snd instrs
         Array.foldiRev (visitInstruction visitor) instrs ctx

  let visitFunction visitor func ctx =
    let ctx, isEnd = visitor.Function ctx func
    if isEnd then ctx
    else visitFunctionTy visitor func.Type ctx
         |> Array.foldiRev (visitBasicBlock visitor) func.Blocks

  let visitGlobal visitor (value: GlobalVar) ctx =
    visitType visitor value.Type ctx |> visitConstantTOpt visitor value.Value

  let visitModule visitor m ctx =
    let types = Map.toArray (Module.getTypes m) |> Array.map snd
    Array.foldRev (visitGlobal visitor) (Module.getGlobals m) ctx
    |> Array.foldRev (visitFunction visitor) (Module.getFunctions m)
    |> Array.foldRev (visitType visitor) types

  let private asyncVisitGlobal visitor ctx global_ = async {
    visitGlobal visitor global_ ctx |> ignore
  }

  let private asyncVisitFunction visitor ctx func = async {
    visitFunction visitor func ctx |> ignore
  }

  let private asyncVisitType visitor ctx ty  = async {
    visitType visitor ty ctx |> ignore
  }

  let pVisitModule visitor m ctx =
    let types = Map.toArray m.Types |> Array.map snd
    Module.getGlobals m
    |> Utils.doParallel (asyncVisitGlobal visitor ctx) |> ignore
    Module.getFunctions m
    |> Utils.doParallel (asyncVisitFunction visitor ctx) |> ignore
    Utils.doParallel (asyncVisitType visitor ctx) types |> ignore
    ctx

  let ignore ctx _ = ctx, true

  let pass ctx _ = ctx, false

  let passI ctx _ _ = ctx, false
