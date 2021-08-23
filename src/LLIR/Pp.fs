module LLIR.Pp

open System

type SB = System.Text.StringBuilder

let private addStr (sb: SB) (str: string) = sb.Append (str) |> ignore
let private addInt (sb: SB) (v: int) = sb.Append (v) |> ignore
let private remove (sb: SB) n = sb.Remove (sb.Length - n, n) |> ignore
let private addTab sb = addStr sb "  "
let private addTab2 sb = addStr sb "    "

let ppBinOp = function
  | ADD  -> "+"
  | SUB  -> "-"
  | MUL  -> "*"
  | UDIV -> "|/|"
  | SDIV -> "/"
  | UREM -> "|%|"
  | SREM -> "%"
  | AND  -> "&&"
  | OR   -> "||"
  | XOR  -> "^^"
  | SHL  -> "<<"
  | LSHR -> "|>>|"
  | ASHR -> ">>"

let ppRelOp = function
  | EQ  -> "="
  | NE  -> "!="
  | ULT -> "|<|"
  | ULE -> "|<=|"
  | UGT -> "|>|"
  | UGE -> "|>=|"
  | SLT -> "<"
  | SLE -> "<="
  | SGT -> ">"
  | SGE -> ">="

let ppUnOp = function
  | NOT -> "!"
  | NEG -> "~"

let ppCastType = function
  | ZEXT -> "zext"
  | SEXT -> "sext"

let ppVarID (id, idx) = sprintf "Var_%d_%d" id idx

let rec ppExpr sb expr =
  match expr with
  | Addr (base_, idx, _) -> ppAddr sb base_ idx
  | Any (exprs, _) -> ppAny sb exprs
  | Arg (idx, size) -> sprintf "Arg_%d<%d>" idx size |> addStr sb
  | BinOp (op, e1, e2, _, _) -> ppBinary sb (ppBinOp op) e1 e2
  | BlockAddr (func, block) -> sprintf "%s[@%d]" func block |> addStr sb
  | Cast (op, e, size, _) -> ppCast sb (ppCastType op) e size
  | Concat (exprs, _, _) -> ppConcat sb exprs
  | Extract (expr, pos, size, _) -> ppExtract sb expr pos size
  | Num bv -> BitVector.toString bv |> addStr sb
  | Load (addr, size, _) -> ppLoad sb addr size
  | LocalAddr idx -> sprintf "LocalAddr_%d" idx |> addStr sb
  | RelOp (op, e1, e2, _) -> ppBinary sb (ppRelOp op) e1 e2
  | Replace (dst, pos, src, _, _) -> ppReplace sb dst pos src
  | Select (c, t,f, _, _) -> ppSelect sb c t f
  | SymVar (id, size) -> sprintf "%s<%d>" id size |> addStr sb
  | Var (id, size) -> sprintf "%s<%d>" (ppVarID id) size |> addStr sb
  | GlobalVar (id, size) -> sprintf "@%s<%d>" id size |> addStr sb
  | Undef (id, size) -> addStr sb (sprintf "Undef_%d<%d>" id size)
  | UnOp (op, e, _, _) -> ppUnary sb (ppUnOp op) e
  | UnIntCall (id, args, _, _) -> ppUnIntCall sb id args
  | Metadata (ptr, _) -> sprintf "MD_%d" ptr |> addStr sb

and ppAddr sb base_ idx =
  addStr sb "Addr("
  ppExpr sb base_
  addStr sb ", "
  ppExpr sb idx
  addStr sb ")"

and ppAny sb exprs =
  addStr sb "Any"
  ppExprs sb exprs

and ppBinary sb op e1 e2 =
  addStr sb "("; ppExpr sb e1; addStr sb ") "
  addStr sb op
  addStr sb " ("; ppExpr sb e2; addStr sb ")"

and ppMem sb addr =
  addStr sb "Mem["
  ppExpr sb addr
  addStr sb "]"

and ppLoad sb addr size =
  ppMem sb addr
  sprintf "<%d>" size |> addStr sb

and ppCast sb op e size =
  addStr sb (sprintf "%s (" op)
  ppExpr sb e
  addStr sb (sprintf ")<%d>" size)

and ppConcat sb exprs =
  if Array.length exprs > 0 then
    let iter expr =
      ppExpr sb expr
      addStr sb " @ "
    addStr sb "("
    Array.iter iter exprs
    remove sb 3
    addStr sb ")"
  else ()

and ppExtract sb expr pos size =
  ppExpr sb expr
  addStr sb "["
  ppExpr sb pos
  addStr sb (sprintf ":%d]" size)

and ppReplace sb dst pos src =
  addStr sb "Replace ("
  ppExpr sb dst
  addStr sb ", "
  ppExpr sb pos
  addStr sb ", "
  ppExpr sb src
  addStr sb ")"

and ppSelect sb c t f =
  addStr sb "("
  ppExpr sb c
  addStr sb " : "
  ppExpr sb t
  addStr sb " ? "
  ppExpr sb f
  addStr sb ")"

and ppUnary sb op e =
  addStr sb (sprintf "(%s(" op)
  ppExpr sb e
  addStr sb "))"

and ppExprs sb exprs =
  let pp expr = ppExpr sb expr; addStr sb ", "
  addStr sb "("
  Array.iter pp exprs
  if Array.length exprs > 0  then remove sb 2
  addStr sb ")"

and ppArg sb expr =
  ppExpr sb expr
  addStr sb ", "

and ppUnIntCall sb id args =
  addStr sb id
  ppExprs sb args

let ppAssign sb id = sprintf "%s := " (ppVarID id) |> addStr sb

let ppAlloca sb id expr =
  ppAssign sb id
  addStr sb "Alloc ("
  ppExpr sb expr
  addStr sb ")"

let ppPhi sb id map =
  let iter blockID expr =
    addStr sb (sprintf "@%d:" blockID)
    ppExpr sb expr
    addStr sb ", "
  ppAssign sb id
  addStr sb "Phi ["
  Map.iter iter map
  if map.Count > 0 then remove sb 2
  addStr sb "]"

let ppStore sb addr value =
  ppMem sb addr
  addStr sb " := "
  ppExpr sb value

let ppCallee sb = function
  | Callee.ID id -> addStr sb (sprintf "%s" id)
  | Callee.Expr expr -> ppExpr sb expr
  | Callee.InlineAsm (code, cons) ->
    addStr sb (sprintf "InlineAsm (\"%s\", \"%s\")" code cons)

let ppCall sb id callee args =
  ppAssign sb id
  ppCallee sb callee
  addStr sb " ("
  Array.iter (ppArg sb) args
  if Array.length args > 0 then remove sb 2
  addStr sb ")"

let ppTry sb id callee args nID eID =
  ppAssign sb id
  ppCallee sb callee
  addStr sb " ("
  Array.iter (ppArg sb) args
  if Array.length args > 0 then remove sb 2
  addStr sb ")"
  sprintf ", normal: @%d, except: @%d"  nID eID |> addStr sb

let ppThrow sb arg =
  addStr sb "Throw "
  ppExpr sb arg

let ppTarget sb idx = addStr sb (sprintf "@%d, " idx)

let ppIndJmp sb expr targets =
  addStr sb "IndJmp "
  ppExpr sb expr
  addStr sb " ["
  Array.iter (ppTarget sb) targets
  if Array.length targets > 0 then remove sb 2
  addStr sb "]"

let ppCondJmp sb cond tID fID =
  addStr sb "CondJmp "
  ppExpr sb cond
  addStr sb ": "
  addStr sb (sprintf "@%d ? @%d" tID fID)

let ppSwitch sb cond cases default_ =
  let iter (expr, blockID) =
    ppExpr sb expr
    addStr sb (sprintf ":@%d, " blockID)
  addStr sb "Switch ("
  ppExpr sb cond
  addStr sb ") ["
  Array.iter iter cases
  if Array.length cases > 0 then remove sb 2
  addStr sb (sprintf "], @%d" default_)

let ppReturn sb exprOpt =
  addStr sb "Ret "
  match exprOpt with
  | Some expr -> ppExpr sb expr
  | None -> ()

let ppStmt sb idx stmt =
  addTab2 sb
  match stmt with
  | Def (id, expr) -> ppAssign sb id; ppExpr sb expr
  | Alloca (id, expr) -> ppAlloca sb id expr
  | Phi (id, map, _, _) -> ppPhi sb id map
  | Store (addr, value) -> ppStore sb addr value
  | Call (id, callee, args, _, _) -> ppCall sb id callee args
  | Try (id, callee, args, _, nID, eID, _) -> ppTry sb id callee args nID eID
  | Throw (arg, _) -> ppThrow sb arg
  | Catch (id, _) -> addStr sb (sprintf "%s = Catch" (ppVarID id))
  | Jmp id -> addStr sb (sprintf "Jmp @%d" id)
  | IndJmp (expr, targets) -> ppIndJmp sb expr targets
  | CondJmp (cond, tID, fID) -> ppCondJmp sb cond tID fID
  | Switch (cond, cases, default_) -> ppSwitch sb cond cases default_
  | Return exprOpt -> ppReturn sb exprOpt
  | UnReachable -> addStr sb "UnReachable"
  | NOP -> addStr sb "NOP"
  addStr sb "\n"

let ppBlock sb idx block =
  addStr sb (sprintf "  @%d:\n" idx)
  Array.iteri (ppStmt sb) block

let ppFunc sb id (func: Function) =
  addStr sb "function "
  addStr sb id
  sprintf " MD_%d" (fst func.Metadata) |> addStr sb
  addStr sb " {\n"
  Array.iteri (ppBlock sb) func.Body
  addStr sb "}\n\n"

let ppGlobal sb id glob =
  addStr sb "@global "
  addStr sb glob.ID
  sprintf " MD_%d" (fst glob.Metadata) |> addStr sb
  if glob.IsWritable then addStr sb " [W] = " else addStr sb " [R] = "
  ppExpr sb glob.Value
  addStr sb "\n"

let ppInstr sb (fID, block, idx) =
  addStr sb (sprintf "@%s[%d:%d], " fID block idx)

let ppCaller sb fID instrs =
  addStr sb (sprintf "\n%s" fID)
  addStr sb "\n  | "
  Set.iter (ppInstr sb) instrs

let ppCallee_ sb instr fIDs =
  let iter fID = addStr sb (sprintf "%s, " fID)
  addStr sb "\n"
  ppInstr sb instr
  addStr sb "\n  | "
  Set.iter iter fIDs

let ppMetadata sb ptr (md: LLVM.Metadata) =
  sprintf "MD_%d = " ptr |> addStr sb
  LLVM.Pp.ppMetadata sb md
  addStr sb "\n"

let ppProg prog =
  let sb = new SB ()
  Map.iter (ppFunc sb) prog.Functions
  Map.iter (ppGlobal sb) prog.Globals
  Map.iter (ppMetadata sb) prog.Metadatas
  (*
  addStr sb "\n\n----- CallerMap -----"
  Map.iter (ppCaller sb) prog.CallerMap
  addStr sb "\n\n----- CalleeMap -----"
  Map.iter (ppCallee_ sb) prog.CalleeMap
  *)
  sb.ToString ()

let printExpr expr =
  let sb = new SB ()
  ppExpr sb expr
  sb.ToString ()

let printExprOpt = function
  | Some expr -> printExpr expr
  | _ -> ""

let printExprs exprs =
  let sb = new SB ()
  ppExprs sb exprs
  sb.ToString ()

let printStmt idx stmt =
  let sb = new SB ()
  ppStmt sb idx stmt
  sb.ToString ()

let printMetadata ptr md =
  let sb = new SB ()
  ppMetadata sb ptr md
  sb.ToString ()
