namespace Common

open System

module File =
  let readLines fname = System.IO.File.ReadLines(fname) |> Seq.toArray

module Tuple =
  let init a b = (a, b)

  let map f (a, b) = (f a, f b)

  let sndMap f (a, b) = (a, f b)

  let third (a, b, c) = c

module Array =
  let revList l = Array.ofList l |> Array.rev

  let ofOpt = function
    | Some arr -> arr
    | _ -> [||]

  let mapState state f arr =
    let folder (state, ret) item =
      let state, item = f state item
      state, item :: ret
    Array.fold folder (state, []) arr |> Tuple.sndMap revList

  let pop arr =
    match Array.length arr with
    | 0 -> failwith "Array.pop fail"
    | len -> Array.get arr 0, Array.sub arr 1 (len - 1)

  let pop2 arr =
    match Array.length arr with
    | 0 | 1 -> failwith "Array.pop2 fail"
    | len -> Array.get arr 0, Array.get arr 1, Array.sub arr 2 (len - 2)

  let foldi folder state arr =
    let fold (state, idx) item =
      let state = folder state idx item
      (state, idx + 1)
    Array.fold fold (state, 0) arr |> fst

  let foldRev folder arr state =
    Array.fold (fun state item -> folder item state) state arr

  let foldiRev folder arr state =
    let fold (state, idx) item =
      let state = folder idx item state
      (state, idx + 1)
    Array.fold fold (state, 0) arr |> fst

  let foldCommon folder acc arr1 arr2 =
    let l1 = Array.length arr1
    let l2 = Array.length arr2
    if l1 = l2 then Array.fold2 folder acc arr1 arr2
    elif l1 < l2 then Array.fold2 folder acc arr1 (Array.sub arr2 0 l1)
    else Array.fold2 folder acc (Array.sub arr1 0 l2) arr2

  let toListMap f arr = Array.map f arr |> Array.toList

  let add arr item = Array.append arr [|item|]

  let last arr =
    let length = Array.length arr
    if length = 0 then failwith "Array.last fail"
    else Array.get arr (length - 1)

  let choosei f arr = Array.mapi f arr |> Array.choose (fun x -> x)

module List=
  let revIter f l = List.rev l |> List.iter f

module Set =
  let init x = Set.add x Set.empty

module Map =
  let ofArrayKeyFunc f arr =
    Array.fold (fun acc item -> Map.add (f item) item acc) Map.empty arr

  let concat maps =
    Array.map Map.toArray maps |> Array.concat |> Map.ofArray

  let getKeys map = Map.toArray map |> Array.map fst

  let findSet key map =
    match Map.tryFind key map with
    | Some set -> set
    | None -> Set.empty

  let private mkValFunc f _ v = f v

  let mapVal f map = Map.map (mkValFunc f) map

  let union maps =
    Array.map Map.toArray maps |> Array.concat |> Map.ofArray

  let private revFolder ret k v = Map.add v k ret
  let rev map = Map.fold revFolder Map.empty map

module Option =
  let apply f opt arg =
    match opt with
    | Some v -> f v arg
    | _ -> arg

module String =
  let endsWith (target:string) src = target.EndsWith (src)

  let startsWith (target:string) src = target.StartsWith (src)

  let subString (target:string) idx = target.Substring (idx)

  let contains (big: string) (small: string) = big.Contains (small)

module ConcurrentDictionary =
  let toMap dic =
    dic :> seq<_>
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

  let ofMap (map: Map<'K, 'V>) =
    let ret = new Collections.Concurrent.ConcurrentDictionary<'K, 'V> ()
    let iter k v = ret.TryAdd (k, v) |> ignore
    Map.iter iter map
    ret

type WeightedList<'T> = {
  Max: int
  Min: int
  Data: Map<int, 'T list>
}

module WeightedList =
  let empty = {
    Max = 0
    Min = 0
    Data = Map.empty
  }

  let add p d l =
    let max_ = if Map.count l.Data = 0 then p else max l.Max p
    let min_ = if Map.count l.Data = 0 then p else min l.Min p
    let data =
      match Map.tryFind p l.Data with
      | Some prev -> Map.add p (d :: prev) l.Data
      | _ -> Map.add p [d] l.Data
    {
      Max = max_
      Min = min_
      Data = data
    }

  let init p d = add p d empty

  let isEmpty l = Map.count l.Data = 0

  let private syncMax l =
    if Map.count l.Data = 0 then l
    else { l with Max = Map.getKeys l.Data |> Array.max }

  let private syncMin l =
    if Map.count l.Data = 0 then l
    else { l with Min = Map.getKeys l.Data |> Array.min }

  let popMax l =
    match Map.tryFind l.Max l.Data with
    | Some [ret] -> ret, syncMax { l with Data = Map.remove l.Max l.Data }
    | Some (ret :: remain) -> ret, { l with Data = Map.add l.Max remain l.Data }
    | _ -> failwith "WeightedList.popMax fail"

  let popMin l =
    match Map.tryFind l.Min l.Data with
    | Some [ret] -> ret, syncMin { l with Data = Map.remove l.Min l.Data }
    | Some (ret :: remain) -> ret, { l with Data = Map.add l.Min remain l.Data }
    | _ -> failwith "WeightedList.popMin fail"

module Timer =
  let start () =
    let timer = new Diagnostics.Stopwatch ()
    timer.Start ()
    timer

  let getTime (timer: Diagnostics.Stopwatch) = timer.ElapsedMilliseconds

[<AutoOpen>]
module tryBuilder =
  let private bind f m = match m with None -> f () | Some _ -> m

  let private forLoop (source: seq<_>) func =
    use ie = source.GetEnumerator ()
    let rec loop () =
      bind (fun () -> if ie.MoveNext () then loop () else None) (func ie.Current)
    if ie.MoveNext () then loop () else None

  type tryBuilder () =
    member __.ReturnFrom (x) = x
    member __.YieldFrom (x) = x
    member __.Bind (m, f) = bind f m
    member __.Delay (f) = f
    member __.Run (f) = f ()
    member __.Combine (m, f) = bind f m
    member __.For (source, computation) = forLoop source computation

  let tryOrNext = tryBuilder ()
