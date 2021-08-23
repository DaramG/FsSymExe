namespace SymExecutor

open Microsoft
open LLIR
open Common

exception PCException of string

type PathConstraint = {
  Paths: (ID * BlockID) list
  PathLen: int
  Conds: Expr list
  CondToPath: int list
  Z3State: Z3State
  Z3Conds: Z3.BoolExpr []
}

module PathConstraint =
  let empty () = {
    Paths = []
    PathLen = 0
    Conds = []
    CondToPath = []
    Z3State = Z3State.init (new Microsoft.Z3.Context ())
    Z3Conds = [||]
  }

  let getCurBB pc =
    match pc.Paths with
    | [] -> -1
    | (_, bb) :: _ -> bb

  let getPrevBB pc =
    match pc.Paths with
    | _ :: (_, bb) :: _ -> bb
    | _ -> PCException "getPrevBB" |> raise

  let addBB pc fname bID =
    { pc with Paths = (fname, bID) :: pc.Paths; PathLen = pc.PathLen + 1 }

  let private addCond pc cond conds z3state =
    { pc with Conds = cond :: pc.Conds
              CondToPath = pc.PathLen :: pc.CondToPath
              Z3Conds = conds
              Z3State = z3state }

  let getSolver pc = function
    | Some solver -> solver
    | _ -> Z3.getSolver pc.Z3State pc.Z3Conds

  let isSAT solver pc cond_ =
    if List.contains cond_ pc.Conds then solver, pc, SAT
    elif List.contains (Builder.mkNot cond_) pc.Conds then solver, pc, UNSAT
    else
      try
        let solver = getSolver pc solver
        let z3state, cond = Z3.exprToZ3Cond pc.Z3State cond_
        let conds = Array.append pc.Z3Conds [|cond|]
        match Z3.isSAT solver cond with
        | SAT -> Some solver, addCond pc cond_ conds z3state, SAT
        | UNSAT -> Some solver, pc, UNSAT
        | TIMEOUT -> Some solver, pc, TIMEOUT
      with
        | Z3Exception _ -> solver, pc, TIMEOUT

  let debug pc =
    let iter expr = Pp.printExpr expr |> printfn "%s"
    printfn "================= PC =========================="
    List.iter iter pc.Conds
    printfn "================= Path ========================"
    List.iter (fun (x, y) -> printfn "%s, %d" x y) pc.Paths

  let isSamePath p1 p2 = p1.PathLen = p2.PathLen && p1.Paths = p2.Paths

  let popCond pc n =
    let idx = n - 1
    let pathLen = List.item idx pc.CondToPath
    {
      pc with Paths = List.skip (pc.PathLen - pathLen) pc.Paths
              PathLen = pathLen
              Conds = List.skip n pc.Conds
              CondToPath = List.skip n pc.CondToPath
              Z3Conds = Array.take (Array.length pc.Z3Conds - n) pc.Z3Conds
    }

  let subSAT pc cond =
    if cond = Builder.False || List.isEmpty pc.Conds then None
    else
      let idx = Z3.getIdxSAT pc.Z3State pc.Z3Conds cond
      popCond pc (Array.length pc.Z3Conds - idx) |> Some

  let getLastZ3Cond pc = Array.last pc.Z3Conds
