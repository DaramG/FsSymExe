namespace SymExecutor

open LLIR
open LLIR.Core
open Common

type Local = Map<int, Expr> list * int

type Memory = {
  Local: Local
  Global: Map<ID, Expr>
  Other: Map<Expr, Expr>
}

module Local =
  let empty = [], 0

  let alloc (maps, num) size =
    let addr = Builder.mkLocalAddr num
    let expr = Builder.mkUndef size
    let maps = (List.head maps |> Map.add num expr) :: List.tail maps
    addr, (maps, num + 1)

  let load (maps, _) idx pos size =
    let rec loop = function
      | map :: remain ->
        match Map.tryFind idx map with
        | Some expr -> Builder.mkExtract expr pos size
        | _ -> loop remain
      | [] -> failwith "Local.load fail"
    loop maps

  let store (maps, num) idx pos value =
    let rec loop = function
      | map :: remain ->
        match Map.tryFind idx map with
        | Some expr ->
          (Map.add idx (Builder.mkReplace expr pos value) map) :: remain
        | _ -> map :: loop remain
      | [] -> failwith "Local.store fail"
    loop maps, num

  let pop (maps, num) = List.tail maps, num

  let push (maps, num) = Map.empty :: maps, num

  let debug local =
    let print idx expr = Pp.printExpr expr |> printfn "%d: %s" idx
    List.iter (Map.iter print) (fst local)

module Memory =
  let empty = {
    Local = Local.empty
    Global = Map.empty
    Other = Map.empty
  }

  let init prog =
    let globs = Program.getGlobals prog |> Map.map (fun _ glob -> glob.Value)
    { empty with Global = globs }

  let alloc mem size =
    let addr, local = Local.alloc mem.Local size
    addr, { mem with Local = local }

  let private loadGlobal globs id pos size =
    match Map.tryFind id globs with
    | Some expr -> Builder.mkExtract expr pos size
    | _ -> failwith "loadGlobal fail"

  let private loadOther other addr size =
    let value =
      match Map.tryFind addr other with
      | Some expr ->
        let exprSize = Builder.getSize expr
        if exprSize = size then expr
        elif exprSize > size then Builder.mkExtract expr Builder.zero32 size
        else
          Builder.mkConcat [|expr; Builder.mkUndef (size - exprSize)|] size
      | _ -> Builder.mkUndef size
    Map.add addr value other, value

  let load mem addr size =
    match addr with
    | LocalAddr idx -> mem, Local.load mem.Local idx Builder.zero32 size
    | Addr (LocalAddr idx, pos, _) -> mem, Local.load mem.Local idx pos size
    | Addr (GlobalVar (id, _), pos, _) -> mem, loadGlobal mem.Global id pos size
    | addr ->
      let other, value = loadOther mem.Other addr size
      { mem with Other = other }, value

  let private storeLocal mem idx pos value =
    { mem with Local = Local.store mem.Local idx pos value }

  let private storeGlobal mem id pos value =
    let globs = mem.Global
    let globs =
      match Map.tryFind id globs with
      | Some expr -> Map.add id (Builder.mkReplace expr pos value) globs
      | _ -> failwith "storeGlobal fail"
    { mem with Global = globs }

  let private storeOther mem addr value =
    let other = mem.Other
    let other =
      match Map.tryFind addr other with
      | Some expr ->
        if Builder.getSize expr > Builder.getSize value then
          Map.add addr (Builder.mkReplace expr Builder.zero32 value) other
        else Map.add addr value other
      | _ -> Map.add addr value other
    { mem with Other = other }

  let store mem addr value =
    match addr with
    | LocalAddr idx -> storeLocal mem idx Builder.zero32 value
    | Addr (LocalAddr idx, pos, _) -> storeLocal mem idx pos value
    | Addr (GlobalVar (id, _), pos, _) -> storeGlobal mem id pos value
    | addr -> storeOther mem addr value

  let initFunc mem = { mem with Local = Local.push mem.Local }

  let doReturn mem = { mem with Local = Local.pop mem.Local }

  let debug mem =
    printfn "Local"
    Local.debug mem.Local
    //printfn "Global"
    //Map.iter (fun id v -> Pp.printExpr v |> printfn "%s: %s" id) mem.Global
    printfn "Other"
    Map.iter (fun a v -> printfn "%s -> %s" (Pp.printExpr a) (Pp.printExpr v))
             mem.Other
