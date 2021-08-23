module Common.Utils

open System
open System.Threading
open System.Threading.Tasks

let doParallel func args =
  Array.map func args |> Async.Parallel |> Async.RunSynchronously

let setTimeout (timeout: int) (func, tag) =
  async {
    use cts = new CancellationTokenSource()
    use timer = Task.Delay (timeout, cts.Token)
    let task = func |> Async.StartAsTask
    let! complete = Async.AwaitTask <| Task.WhenAny (task, timer)
    if complete = (task:> Task) then
      cts.Cancel ()
      let! result = Async.AwaitTask task
      return Some result, tag
    else return None, tag
  }

let doParallel2 cores tasks =
  let length = Array.length tasks
  let map n =
    let idx = n * cores
    let cnt = if length >= idx + cores then cores else length - idx
    Array.sub tasks idx cnt
    |> Async.Parallel
    |> Async.RunSynchronously
  let cnt = if length % cores = 0 then (length / cores) - 1 else length / cores
  Array.map map [|0 .. cnt|] |> Array.concat

let doParallelTimeout cores timeout tasks =
  let length = Array.length tasks
  let map n =
    let idx = n * cores
    let cnt = if length >= idx + cores then cores else length - idx
    Array.sub tasks idx cnt |> Array.map (setTimeout timeout)
    |> Async.Parallel
    |> Async.RunSynchronously
  let cnt = if length % cores = 0 then (length / cores) - 1 else length / cores
  Array.map map [|0 .. cnt|] |> Array.concat
