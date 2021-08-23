namespace LLIR

open LLIR.Builder

type Replacer = {
  TryFindExpr: Expr -> Expr option
  FindMem: Expr -> int -> Expr
}

module Replacer =

  let private noOpMem addr size = mkLoad addr size

  let init findExpr findMem = {
    TryFindExpr = findExpr
    FindMem = findMem
  }

  let initOneExpr target value = {
    TryFindExpr = (fun expr -> if expr = target then Some value else None)
    FindMem = noOpMem
  }

  let rec doReplaceExpr replacer expr =
    match replacer.TryFindExpr expr with
    | Some value -> value
    | _ ->
      let replace e = doReplaceExpr replacer e
      match expr with
      | Arg _
      | BlockAddr _
      | Num _
      | LocalAddr _
      | SymVar _
      | Var _
      | GlobalVar _
      | Undef _  -> expr
      | Any (exprs, size) -> mkAny (replaceExprs replacer exprs) size
      | Addr (b, i, _) -> mkAddr (replace b) (replace i)
      | BinOp (op, e1, e2, size, _) -> mkBinOp op (replace e1) (replace e2) size
      | Cast (ty, e, size, _) -> mkCast ty (replace e) size
      | Concat (exprs, size, _) -> mkConcat (replaceExprs replacer exprs) size
      | Extract (e1, e2, size, _) -> mkExtract (replace e1) (replace e2) size
      | Load (e, size, _) -> mkLoad (replace e) size
      | RelOp (op, e1, e2, _) -> mkRelOp op (replace e1) (replace e2)
      | Replace (e1, e2, e3, _, _) ->
        mkReplace (replace e1) (replace e2) (replace e3)
      | Select (e1, e2, e3, size, _) ->
        mkSelect (replace e1) (replace e2) (replace e3) size
      | UnIntCall (callee, args, size, _) ->
        mkUnIntCall callee (replaceExprs replacer args) size
      | UnOp (op, e, size, _) -> mkUnOp op (replace e) size
      | Metadata _ -> expr

  and replaceExprs replacer exprs = Array.map (doReplaceExpr replacer) exprs

  let replaceExpr replacer expr = doReplaceExpr replacer expr

  let rec replaceMem replacer expr =
    let replace e = replaceMem replacer e
    match expr with
    | Arg _
    | BlockAddr _
    | Num _
    | LocalAddr _
    | SymVar _
    | Var _
    | GlobalVar _
    | Undef _  -> expr
    | Any (exprs, size) -> mkAny (replaceMems replacer exprs) size
    | Addr (b, i, _) -> mkAddr (replace b) (replace i)
    | BinOp (op, e1, e2, size, _) -> mkBinOp op (replace e1) (replace e2) size
    | Cast (ty, e, size, _) -> mkCast ty (replace e) size
    | Concat (exprs, size, _) -> mkConcat (replaceMems replacer exprs) size
    | Extract (e1, e2, size, _) -> mkExtract (replace e1) (replace e2) size
    | Load (e, size, _) -> replacer.FindMem (replace e) size
    | RelOp (op, e1, e2, _) -> mkRelOp op (replace e1) (replace e2)
    | Replace (e1, e2, e3, _, _) ->
      mkReplace (replace e1) (replace e2) (replace e3)
    | Select (e1, e2, e3, size, _) ->
      mkSelect (replace e1) (replace e2) (replace e3) size
    | UnIntCall (callee, args, size, _) ->
      mkUnIntCall callee (replaceMems replacer args) size
    | UnOp (op, e, size, _) -> mkUnOp op (replace e) size
    | Metadata _ -> expr

  and replaceMems replacer exprs = Array.map (replaceMem replacer) exprs

  let replace replacer expr = replaceExpr replacer expr |> replaceMem replacer
