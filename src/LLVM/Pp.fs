module LLVM.Pp

open System.Text

type SB = StringBuilder

let private addStr (sb: SB) (str: string) = sb.Append (str) |> ignore

let floatTyToStr = function
  | LLVM.FloatTy.Half -> "Half"
  | LLVM.FloatTy.Float -> "Float"
  | LLVM.FloatTy.Double -> "Double"
  | LLVM.FloatTy.Fp128 -> "Fp128"
  | LLVM.FloatTy.X86Fp80 -> "X86Fp80"
  | LLVM.FloatTy.PpcFp128 -> "PpcFp128"

let typeToStr sb ty = failwith "todo"

let consToStr sb cons = failwith "todo"

let instrToStr sb instr = failwith "todo"

let bbToStr sb bb = failwith "todo"

let funcToStr sb func = failwith "todo"

let glbVarToStr sb var = failwith "todo"

let moduleToStr m = failwith "todo"

let ppRefs refs = Array.map (sprintf "%d") refs |> String.concat ","

let ppMetadata sb = function
  | Metadata.LocalVar (name, ty) ->
    sprintf "LocalVar {name: '%s', ty: %d}" name ty |> addStr sb
  | Metadata.DerivedType (kind, name, ty, offset) ->
    sprintf "DerivedType {kind: %A, name: '%s', ty: %d, offset: %d}" kind name
            ty offset
    |> addStr sb
  | Metadata.CompositeType (kind, name, id, params_, elems, offset) ->
    sprintf "CompositeType {kind: %A, name: '%s', id: '%s', params: %s, elems: %s, offset: %d}"
            kind name id (ppRefs params_) (ppRefs elems) offset
    |> addStr sb
  | Metadata.LocalAs (_, name, varID) ->
    sprintf "LocalAs {name: '%s', varID: %A}" name varID |> addStr sb
  | Metadata.SubProgram (name, link, ty, idx) ->
    sprintf "SubProgram {name: '%s', link: '%s', type: %d, vidx: %A}" name link
             ty idx
    |> addStr sb
  | Metadata.SubRoutine types ->
    sprintf "SubRoutine {types: %s}" (ppRefs types) |> addStr sb
  | Metadata.BasicType (name, size) ->
    sprintf "BasicType {name: %s, size: %d}" name size |> addStr sb
  | Metadata.VOID -> addStr sb "Void"
  | Metadata.TODO -> addStr sb "TODO"
