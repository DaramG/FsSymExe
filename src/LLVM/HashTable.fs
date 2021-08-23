namespace LLVM

open System
open System.Threading

type HashTable<'T when 'T: equality> () =
  let CAP = 1024
  let mutable cnt = 0
  let mutable cap = 0
  let mutable table: 'T [] = [||]
  let lock = new Object ()

  member private __.Add (factory, item) =
    let item = factory cnt item
    if cnt = cap then
      table <- Array.create CAP item |> Array.append table
      cnt <- cnt + 1
      cap <- cap + CAP
    else
      table.[cnt] <- item
      cnt <- cnt + 1
    item

  member __.AddHash (factory, item) =
    Monitor.Enter (lock)
    let ret = __.Add (factory, item)
    Monitor.Exit (lock)
    ret

  member __.GetOrAddHash (factory, item) =
    Monitor.Enter (lock)
    let ret =
      match Array.tryFind (fun x -> x = item) table with
      | Some x -> x
      | None -> __.Add (factory, item)
    Monitor.Exit (lock)
    ret

  member __.Get ty = Array.find (fun x -> x = ty) table
