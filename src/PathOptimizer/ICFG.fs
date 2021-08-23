namespace PathOptimizer

open System.Collections

open LLIR
open LLIR.Core
open Common

type ConstArgs = Map<int, Expr>
type Edges = Map<BlockID, Set<BlockID>>
type RetConst = Expr * bool

type ICFG<'T> = {
  ID: ID
  Args: ConstArgs
  Key: 'T
  Edges: Edges
  Calls: Map<BlockID * int, ICFG<'T>>
  HasRet: bool
  RetVal: Expr option
  RetNodes: Set<BlockID>
  RetConst: RetConst option
  IsRec: bool
  IsMerged: bool
}

type HashICFG<'T> = ID * ConstArgs * Edges * 'T * RetConst option
type CacheICFG<'T> = Concurrent.ConcurrentDictionary<HashICFG<'T>, ICFG<'T>>

module ConstArgs =
  let empty = Map.empty

  let private mkArgZEXT idx fSize tSize =
    let arg = Builder.mkArg idx fSize
    Builder.mkCast ZEXT arg tSize

  let private ofExpr (ret, idx) expr =
    match expr with
    | Num _ -> Map.add idx expr ret, idx + 1
    | Cast (ZEXT, e, size, _) ->
      let expr = mkArgZEXT idx (Builder.getSize e) size
      Map.add idx expr ret, idx + 1
    | _ -> ret, idx + 1

  let ofExprs exprs = Array.fold ofExpr (Map.empty, 0) exprs |> fst

  let toArgs prog cArgs =
    let mapi idx arg =
      match Map.tryFind idx cArgs with
      | Some (Cast (ZEXT, e, size, _)) -> mkArgZEXT idx (Builder.getSize e) size
      | Some expr -> expr
      | _ -> Expr.Arg arg
    Function.getArgs prog |> Array.mapi mapi

  let debug pad cArgs =
    let iter idx arg = Pp.printExpr arg |> printfn "%s | Arg[%d] = %s" pad idx
    Map.iter iter cArgs

module ICFG =

  let getKey cfg = cfg.Key
  let getRetConst cfg = cfg.RetConst
  let getID cfg = cfg.ID
  let getHash cfg = cfg.ID, cfg.Args, cfg.Edges, cfg.Key, cfg.RetConst

  let printRetConstOpt = function
    | Some (expr, flag) -> sprintf "%s, %A" (Pp.printExpr expr) flag
    | _ -> ""

  let debugN n cfg =
    let padStr = "  "
    let printSet set = Set.iter (printf "%d, ") set
    let rec iter idx pad cfg =
      if idx < 0 then ()
      else
        printfn "%s|---------------------------" pad
        printfn "%s| ID: %s" pad cfg.ID
        printfn "%s| key: %A" pad cfg.Key
        printfn "%s| IsRec: %A" pad cfg.IsRec
        printfn "%s| IsMerged: %A" pad cfg.IsMerged
        printfn "%s| HasRet: %A" pad cfg.HasRet
        Pp.printExprOpt cfg.RetVal |> printfn "%s| RetVal: %s" pad
        printRetConstOpt cfg.RetConst |> printfn "%s| RetConst: %s" pad
        printfn "%s| Args" pad
        ConstArgs.debug pad cfg.Args
        printf "%s| RetNodes: " pad
        printSet cfg.RetNodes
        printf "\n%s| Edges:" pad
        Map.iter (fun k set -> printf "\n%s| %d: " pad k; printSet set) cfg.Edges
        printfn ""
        Map.iter (fun k cfg -> printfn "%s| %A" pad k; iter (idx - 1) (pad + padStr) cfg)
               cfg.Calls
    iter n "" cfg

  let debug cfg = debugN 10 cfg

  let setArgs (cfg: ICFG<'T>) args = { cfg with Args = args }

  let setRetConst retConst cfg = { cfg with RetConst = retConst }

  let getRetVal cfg = cfg.RetVal

  let setRetVal (cfg: ICFG<'T>) value =
    match value with
    | Num _ -> { cfg with RetVal = Some value }
    | _ -> cfg

  let dummy fID key = {
    ID = fID
    Args = Map.empty
    Key = key
    Edges = Map.empty
    Calls = Map.empty
    RetNodes = Set.empty
    HasRet = true
    RetVal = None
    RetConst = None
    IsRec = false
    IsMerged = false
  }

  let empty fID key = {
    ID = fID
    Args = Map.empty
    Key = key
    Edges = Map.empty
    Calls = Map.empty
    RetNodes = Set.empty
    HasRet = false
    RetVal = None
    RetConst = None
    IsRec = false
    IsMerged = false
  }

  let dummyRec fID key args retConst = {
    ID = fID
    Args = args
    Key = key
    Edges = Map.empty
    Calls = Map.empty
    RetNodes = Set.empty
    HasRet = true
    RetVal = None
    RetConst = retConst
    IsRec = true
    IsMerged = false
  }

  let ofFunc key func = {
    ID = Function.getID func
    Args = Map.empty
    Key = key
    Edges = Function.getEdges func
    Calls = Map.empty
    RetNodes = Function.getRetNodes func
    HasRet = true
    RetVal = None
    RetConst = None
    IsRec = false
    IsMerged = false
  }

  let hasRet cfg = cfg.HasRet

  let addRet bID cfg =
    { cfg with RetNodes = Set.add bID cfg.RetNodes
               HasRet = true }

  let getFromNodes cfg node =
    let choose (fr, set) = if Set.contains node set then Some fr else None
    Map.toArray cfg.Edges |> Array.choose choose |> Set.ofArray

  let getEdges cfg = cfg.Edges

  let hasEdge (cfg: ICFG<'T>) src dst =
    Map.findSet src cfg.Edges |> Set.contains dst

  let setEdge cfg src dsts =
    let edges = cfg.Edges
    if dsts = Set.empty then { cfg with Edges = Map.remove src edges }
    else { cfg with Edges = Map.add src dsts edges }

  let setEdges edges (cfg: ICFG<'T>) =
    let nodes = Function.edgesToNodes edges |> Set.add 0
    let calls = Map.filter (fun (bID, _) _ -> Set.contains bID nodes) cfg.Calls
    let retNodes = Set.intersect nodes cfg.RetNodes
    { cfg with Edges = edges
               Calls = calls
               HasRet = Set.isEmpty retNodes |> not
               RetNodes = retNodes }

  let removeEdge cfg src dst =
    let edges = cfg.Edges
    let set = Map.findSet src edges |> Set.remove dst
    let edges =
      if set = Set.empty then Map.remove src edges
      else Map.add src set edges
    { cfg with Edges = edges }

  let removeNode cfg bID =
    let filter (kbID, _) _ = bID <> kbID
    let retNodes = Set.remove bID cfg.RetNodes
    { cfg with Edges = Function.removeNode cfg.Edges bID
               Calls = Map.filter filter cfg.Calls
               RetNodes = retNodes
               HasRet = Set.count retNodes > 0 }

  let addCall (cfg: ICFG<'T>) key v =
    { cfg with Calls = Map.add key v cfg.Calls }

  let tryGetCall (cfg: ICFG<'T>) key = Map.tryFind key cfg.Calls

  let resolve (cache: CacheICFG<'T>) cfg =
    if cfg.IsRec then
      let key = getHash cfg
      match cache.TryGetValue key with
      | true, cfg -> Some cfg
      | _ -> None
    else Some cfg

  let isRec cfg = cfg.IsRec

  let fini (cfg: ICFG<'T>) =
    let edges =
      Function.calcReachableEdges (Set.init 0) cfg.Edges
      |> Function.reduceReachableEdge cfg.RetNodes
    let nodes = Map.fold (fun ret b s -> (Set.add b ret) + s) Set.empty edges
                |> Set.add 0
    let calls = Map.filter (fun (b, _) _ -> Set.contains b nodes) cfg.Calls
    let retNodes = Set.intersect nodes cfg.RetNodes
    { cfg with Edges = edges; Calls = calls; RetNodes = retNodes;
               HasRet = Set.count retNodes > 0 }

  let isReachable cfg (sBid, sIid) (dBid, dIid) =
    let edge = cfg.Edges
    let rec loop doneSet = function
      | cur :: remain when Set.contains cur doneSet -> loop doneSet remain
      | cur :: remain ->
        if Set.count doneSet > 0 && cur = dBid then true
        else
          let nxts = Map.findSet cur edge |> Set.toList
          loop (Set.add cur doneSet) (nxts @ remain)
      | [] -> Set.contains dBid doneSet

    if sBid = dBid && dIid > sIid then true
    else loop Set.empty [sBid]

  let mergeEdges e1 e2 =
    let mergeEdge ret src dsts = Map.add src ((Map.findSet src ret) + dsts) ret
    Map.fold mergeEdge e1 e2

  let mergeRetVal v1 v2 = if v1 = v2 then v1 else None

  let rec merge (cfg1: ICFG<'T>) (cfg2: ICFG<'T>) =
    if cfg1.ID <> cfg2.ID then failwith "mergeICFG fail"
    if cfg1.Edges = cfg2.Edges && cfg1.Args = cfg2.Args
       && not cfg1.IsMerged && not cfg2.IsMerged then cfg1
    else
      { cfg1 with IsMerged = true
                  Edges = mergeEdges cfg1.Edges cfg2.Edges
                  Calls = mergeCalls cfg1.Calls cfg2.Calls
                  HasRet = cfg1.HasRet || cfg2.HasRet
                  RetVal = mergeRetVal cfg1.RetVal cfg2.RetVal
                  RetNodes = cfg1.RetNodes + cfg2.RetNodes
                  IsRec = cfg1.IsRec || cfg2.IsRec
      }

  and mergeCalls call1 call2 =
    let mergeCall ret instPos cfg2 =
      match Map.tryFind instPos ret with
      | Some cfg1 -> Map.add instPos (merge cfg1 cfg2) ret
      | _ -> Map.add instPos cfg2 ret
    Map.fold mergeCall call1 call2

  let isMerged cfg = cfg.IsMerged

  let isEqual c1 c2 = c1.Edges = c2.Edges && c1.RetNodes = c2.RetNodes
