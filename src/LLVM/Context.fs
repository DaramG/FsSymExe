module LLVM.Context

open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices

type CPtr = uint64

type Context = {
  TypeCache: Concurrent.ConcurrentDictionary<CPtr, Type>
  OperandCache: Concurrent.ConcurrentDictionary<CPtr, Operand>
  ConstantCache: Concurrent.ConcurrentDictionary<CPtr, ConstantT>
  TypeIDs: ID list
  Track: CPtr
  Module: CPtr
  Func: CPtr
  Types: Map<ID, Type>
  Metadatas: Metadatas
  MetadataRefs: Dictionary<CPtr, MetadataRef>
}

module Context =

  [<DllImport("llvm\libllvm.dll")>]
  extern CPtr getTracker (CPtr);

  [<DllImport("llvm\libllvm.dll")>]
  extern void setTrackerFunc (CPtr, CPtr);

  let private initRefs () =
    let ret = new Dictionary<CPtr, MetadataRef> ()
    ret.Add (0UL, 0)
    ret.Add (1UL, 1)
    ret

  let private MDS = [| (0, Metadata.TODO); (1, Metadata.VOID) |]

  let empty = {
    TypeCache = new Concurrent.ConcurrentDictionary<CPtr, Type> ()
    OperandCache = new Concurrent.ConcurrentDictionary<CPtr, Operand> ()
    ConstantCache = new Concurrent.ConcurrentDictionary<CPtr, ConstantT> ()
    TypeIDs = []
    Track = 0UL
    Module = 0UL
    Func = 0UL
    Types = Map.empty
    Metadatas = Map.ofArray MDS
    MetadataRefs = initRefs ()
  }

  let getType ctx ptr = ctx.TypeCache.TryGetValue ptr

  let addTypes (ctx: Context) id ty =
    { ctx with Types = Map.add id ty ctx.Types }

  let addType ctx ptr ty =
    let ty = ctx.TypeCache.GetOrAdd (ptr, AST.calcTyHash ty)
    match ty with
    | Type.Struct ((id, _, _, _, _), _) -> addTypes ctx id ty, ty
    | _ -> ctx, ty

  let pushTyID ctx id = { ctx with TypeIDs = id :: ctx.TypeIDs }

  let popTyID ctx = { ctx with TypeIDs = List.tail ctx.TypeIDs }

  let containsTyID ctx id = List.contains id ctx.TypeIDs

  let setFuncPtr ctx ptr =
    setTrackerFunc (ctx.Track, ptr)
    { ctx with Func = ptr }

  let getFuncPtr ctx = ctx.Func

  let getTrack ctx = ctx.Track

  let getModule ctx = ctx.Module

  let addOperand ptr (ctx, op) =
    ctx.OperandCache.TryAdd (ptr, op) |> ignore
    ctx, op

  let getOperand ctx ptr = ctx.OperandCache.TryGetValue ptr

  let addConstantT ctx ptr cons =
    ctx.ConstantCache.TryAdd (ptr, cons) |> ignore
    ctx, cons

  let getConstantT ctx ptr = ctx.ConstantCache.TryGetValue ptr

  let initTypes (ctx: Context) = { ctx with Types = Map.empty }

  let init ctx ptr =
    { ctx with Track = getTracker ptr
               Types = Map.empty
               Module = ptr }

  let getTypes ctx = ctx.Types

  let toMDRef ctx ptr =
    match ctx.MetadataRefs.TryGetValue ptr with
    | true, r -> r
    | _ -> failwith "toMDRef fail"

  let reserveMD ctx ptr =
    let refs = ctx.MetadataRefs
    if refs.ContainsKey ptr then ctx
    else
      refs.Add (ptr, refs.Count)
      ctx

  let getMetadata ctx ptr = Map.find (toMDRef ctx ptr) ctx.Metadatas

  let addMetadata (ctx: Context) ptr md =
    { ctx with Metadatas = Map.add (toMDRef ctx ptr) md ctx.Metadatas }

  let hasMetadata ctx ptr = ctx.MetadataRefs.ContainsKey ptr

  let debug ctx =
    Map.iter (printfn "%d: %A") ctx.Metadatas

  let getMetadatas ctx = ctx.Metadatas
