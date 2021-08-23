namespace LLIR.Core

open LLIR
open Common

module Expr =
  let isUndef = function
    | Undef _ -> true
    | _ -> false

module Stmt =
  let chooseFunc = function
    | Call (_, Callee.ID id, _, _, _)
    | Try (_, Callee.ID id, _, _, _, _, _) -> Some id
    | _ -> None

  let isRet = function
    | Return _ -> true
    | _ -> false

module Function =
  let getID (func: Function) = func.ID

  let private toBackEdge ret src dsts =
    let folder ret dst =
      match Map.tryFind dst ret with
      | Some set -> Map.add dst (Set.add src set) ret
      | None -> Map.add dst (Set.init src) ret
    Set.fold folder ret dsts

  let getReachables nodes backEdge =
    let rec loop doneSet = function
      | [] -> doneSet
      | item :: remain ->
        if Set.contains item doneSet then loop doneSet remain
        else
          let nxts = Map.findSet item backEdge |> Set.toList
          loop (Set.add item doneSet) (nxts @ remain)
    loop Set.empty (Set.toList nodes)

  let calcReachableEdges froms edges =
    let find x = Map.findSet x edges
    let add ret x =
      match Map.tryFind x edges with
      | Some set -> Map.add x set ret
      | _ -> ret
    let rec loop todoSet ret =
      if todoSet = Set.empty then ret
      else
        let ret = Set.fold add ret todoSet
        let nxts =
          Set.map find todoSet |> Set.unionMany
          |> Set.filter (fun x -> Map.containsKey x ret |> not)
        loop nxts ret
    loop froms Map.empty

  let calcBackEdges edges = Map.fold toBackEdge Map.empty edges

  let calcReachableNodes froms edges =
    let n1 = getReachables froms edges
    let n2 = calcBackEdges edges |> getReachables froms
    Set.unionMany [| froms; n1; n2 |]

  let removeNode edges node =
    let edges = Map.remove node edges
    let map _ set = Set.remove node set
    Map.map map edges

  let mergeEdges edges1 edges2 =
    let folder ret n2 s2 =
      match Map.tryFind n2 ret with
      | Some s -> Map.add n2 (Set.union s s2) ret
      | _ -> Map.add n2 s2 ret
    Map.fold folder edges1 edges2

  let stripEdge (fID, func, nodes) =
    let nodes = calcBackEdges func.Edges |> getReachables nodes
    let folder ret src dsts =
      if Set.contains src nodes then
        let dsts = Set.filter (fun x -> Set.contains x nodes) dsts
        if dsts = Set.empty then ret
        else Map.add src dsts ret
      else ret
    fID, { func with Edges = Map.fold folder Map.empty func.Edges; Nodes = nodes }

  let reduceReachableEdge nodes edges =
    let fEdges = calcReachableEdges nodes edges
    let bEdges = calcBackEdges edges
                 |> calcReachableEdges nodes |> calcBackEdges
    mergeEdges fEdges bEdges

  let reduceDirectedEdge targets edges =
    calcBackEdges edges |> calcReachableEdges targets |> calcBackEdges

  let edgesToSet edges =
    let map (fr, tos) = Set.map (fun to_ -> fr, to_) tos
    Map.toArray edges |> Array.map map |> Set.unionMany

  let edgesToNodes edges =
    let fold ret fr tos = Set.add fr (ret + tos)
    Map.fold fold Set.empty edges

  let getNodes func = func.Nodes

  let getRetNodes func = func.RetNodes
    (*
    let body = func.Body
    let hasRet bID = Array.get body bID |> Array.exists Stmt.isRet
    getNodes func |> Set.filter hasRet
    *)

  let setNodes nodes func = { func with Nodes = nodes }

  let getEdges func = func.Edges
  let setEdges edges func = { func with Edges = edges }

  let getArgs func = func.Args
  let getArgc func = Array.length func.Args

  let getBody func = func.Body

  let getBB func idx = Array.get func.Body idx

  let hasNode func idx = Set.contains idx func.Nodes

  let hasEdge func fr to_ =
    match Map.tryFind fr func.Edges with
    | Some set -> Set.contains to_ set
    | _ -> false

  let isDeclr func = func.IsDeclr

  let private isReachableBlock block =
    match Array.last block with
    | UnReachable -> false
    | _ -> true

  let stripUnreachable func =
    let blocks = Array.map isReachableBlock func.Body
    let nodes = func.Nodes
    let nodes_ = Set.filter (Array.get blocks) nodes
    let edges = Set.fold removeNode func.Edges (nodes - nodes_)
    setNodes nodes_ func |> setEdges edges

  let printEdges func =
    let iter fr tos =
      printf "%d ->" fr
      Set.iter (printf "%d, ") tos
      printfn ""
    Map.iter iter func.Edges

  let foldStmt folder acc func =
    let helper acc block = Array.fold folder acc block
    getBody func |> Array.fold helper acc

  let foldStmtIdx folder acc func =
    let helper acc bID block =
      let helper acc iID stmt = folder acc (bID, iID) stmt
      Array.foldi helper acc block
    getBody func |> Array.foldi helper acc

  let getArgsMD (func: Function) = (snd func.Metadata).Args

  let getRetMD (func: Function) = (snd func.Metadata).RetVal

  let getCalls (func: Function) = func.Calls

module Program =
  let private triage ret (fID, bID, _) =
    match Map.tryFind fID ret with
    | Some set -> Map.add fID (Set.add bID set) ret
    | None -> Map.add fID (Set.init bID) ret

  let isDeclr prog id = Map.find id prog.Functions |> Function.isDeclr

  let hasFunc prog id = Map.containsKey id prog.Functions

  let getFunc prog id = Map.find id prog.Functions

  let tryGetFunc prog id = Map.tryFind id prog.Functions

  let getGlobalMD prog id = (Map.find id prog.Globals).Metadata
  let getGlobal prog id = (Map.find id prog.Globals).Value
  let getGlobals prog = prog.Globals
  let setGlobals prog globs = { prog with Globals = globs }

  let getGlobalStr prog id =
    match getGlobal prog id with
    | Num bv -> BitVector.convertToString bv
    | _ -> failwith "getGlobalStr fail"

  let getFuncs prog = prog.Functions

  let setFuncs prog funcs = { prog with Functions = funcs }

  let stripUnreachable prog =
    let map _ f = Function.stripUnreachable f
    { prog with Functions = Map.map map prog.Functions }

  let getMD (prog: Program) ptr = Map.find ptr prog.Metadatas
  let getMDs (prog: Program) = prog.Metadatas

  let getFuncMD prog id =
    let func = getFunc prog id
    let ptr = fst func.Metadata
    ptr, getMD prog ptr

module Metadata =
  open LLVM

  let isVoid = function
    | VOID -> true
    | _ -> false

  let isReducedKind = function
    | Ptr
    | PtrToMember
    | Ref
    | RValRef -> false
    | Member
    | TypeDef
    | Inherit
    | Friend
    | Const
    | Volatile
    | Restrict
    | Atomic -> true

  let private findComposite prog kind name =
    let pick ptr = function
      | CompositeType (k, n, _, p, e, _) as md ->
        if k = kind && n = name && (p <> [||] || e <> [||]) then Some ptr
        else None
      | _ -> None
    match Map.tryPick pick (Program.getMDs prog) with
    | Some ret -> ret
    | _ -> fst AST.todoMD

  let reduceMD prog (ptr_, md) =
    let getMD ptr = if ptr = ptr_ then md else Program.getMD prog ptr
    let cache = prog.CacheReduceMD
    let rec loop ptr = cache.GetOrAdd (ptr, calc)
    and calc ptr =
      match getMD ptr with
      | LocalVar (_, ptr) -> loop ptr
      | DerivedType (kind, _, ptr, _) when isReducedKind kind ->
        loop ptr
      | CompositeType (Class, name, _, [||], [||], _) when name <> "" ->
        findComposite prog Class name
      | CompositeType (Structure, name, _, [||], [||], _) when name <> "" ->
        findComposite prog Structure name
      | _ -> ptr
    let ptr = loop ptr_
    ptr, getMD ptr

  let isSubMD prog large small =
    let getMD ptr = reduceMD prog (ptr, Program.getMD prog ptr)
    let cache = prog.CacheSubMD

    let rec check (t1, t2) ret (p1, p2) =
      match ret with
      | Some true -> ret
      | _ ->
        if t1 = p1 then
          if t2 = p2 then Some true else Some false
        else ret

    let rec loop trace large small =
      cache.GetOrAdd ((large, small), fun (l, s) -> calc trace l s)

    and calc trace large small =
      let p1, m1 = getMD large
      let p2, m2 = getMD small
      if p1 = p2 then true
      else
        match List.fold (check (p1, p2)) None trace with
        | Some ret -> ret
        | _ -> compare ((p1, p2) :: trace) (p1, m1) (p2, m2)

    and compare trace (p1, m1) (p2, m2) =
      match m1, m2 with
      | LocalVar (_, r1), LocalVar (_, r2) -> loop trace r1 r2
      | DerivedType (k1, _, r1, _), DerivedType (k2, _, r2, _) when k1 = k2 ->
        loop trace r1 r2
      | CompositeType (k1, _, _, p1s, r1s, _), CompositeType (k2, _, _, p2s, r2s, _) ->
        ((isInherits trace p1 r2s) || (loops trace p1s p2s && loops trace r1s r2s))
      | _, CompositeType (_, _, _, _, refs, _) when isInherits trace p1 refs ->
        true
      | SubProgram (_, _, r1, _), SubProgram (_, _, r2, _) -> loop trace r1 r2
      | SubRoutine r1, SubRoutine r2 -> loops trace r1 r2
      | _ -> m1 = m2

    and isInherits trace p1 refs =
      let isInherit ptr =
        match Program.getMD prog ptr with
        | DerivedType (Inherit, _, p, _) -> loop (List.tail trace) p1 p
        | _ -> false
      Array.exists isInherit refs

    and loops trace l s =
      Array.length l = Array.length s && Array.forall2 (loop trace) l s

    loop [] large small
