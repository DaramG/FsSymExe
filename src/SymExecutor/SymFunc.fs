namespace SymExecutor

open LLIR

module SymFunc =
  let skip fID state _ retID args size =
    State.mkUnIntCall state fID args size ||> State.addVar retID |> Some

  let mkSkip fID = fID, skip fID

  let exit fID _ _ _ _ _ = None

  let mkExit fID = fID, exit fID

  let strlen state _ retID args retSize =
    let ptr = Array.get args 0
    let rec loop idx =
      let n = Builder.ofInt64 (8L * idx)
      match State.load 8 state (Builder.mkAddr ptr n) with
      | _, Num bv ->
        if BitVector.isZero bv then state, Builder.ofInt64 idx
        else loop (idx + 1L)
      | _ -> State.mkUnIntCall state "strlen" args retSize

    loop 0L ||> State.addVar retID |> Some

  let private tryFindCommon state fID =
    match fID with
    | "strlen" -> Some strlen
    | _ ->
      if fID.StartsWith("abort") then Some (exit fID)
      elif State.isDeclr state fID then Some (skip fID)
      else None

  let tryFind state (fID: string) =
    match tryFindCommon state fID with
    | Some func -> Some func
    | _ -> state.GetSymFunc state fID
