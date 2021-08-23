namespace LLIR

open Common
open LLVM
open System
open System.Collections

// if Length > 64 then use BigNum else Num
type BitVector = {
  Length: int
  Num: uint64
  BigNum: bigint // always unsigned
}

exception BitVectorException of string

module BitVector =
  let private max8  = 0xffUL
  let private max16 = 0xffffUL
  let private max32 = 0xffffffffUL
  let private max64 = 0xffffffffffffffffUL
  let private maxArr =
    Array.append (Array.init 64 (fun n -> (1UL <<< n) - 1UL)) [|max64|]
  let private maxDict = new Concurrent.ConcurrentDictionary<int, bigint> ()

  let private sMax8  = 0x7fUL
  let private sMax16 = 0x7fffUL
  let private sMax32 = 0x7fffffffUL
  let private sMax64 = 0x7fffffffffffffffUL
  let private sMaxArr = Array.init 64 (fun len -> (1UL <<< len) - 1UL)
  let private sMaxDict = new Concurrent.ConcurrentDictionary<int, bigint> ()

  let private sMin8  = 0x80UL
  let private sMin16 = 0x8000UL
  let private sMin32 = 0x80000000UL
  let private sMin64 = 0x8000000000000000UL
  let private sMinArr = Array.init 64 (fun len -> 1UL <<< len)
  let private sMinDict = new Concurrent.ConcurrentDictionary<int, bigint> ()

  let initSmall len num = {
    Length = len
    Num = num
    BigNum = bigint.Zero
  }

  let private bigPowDict = new Concurrent.ConcurrentDictionary<int, bigint> ()
  let private mkBigPow len = bigint.Pow (2I, len)
  let private getBigPow len = bigPowDict.GetOrAdd (len, mkBigPow)

  let private mkBigMax len = bigint.Pow (2I, len) - bigint.One
  let private getBigMax len = maxDict.GetOrAdd (len, mkBigMax)
  let private getMax = function
    | len when len <= 64 && 0 < len -> Array.get maxArr len
    | len -> sprintf "Invalid length: %d" len |> BitVectorException |> raise

  let private initSmallCheck len num =
    if len > 64 then
      sprintf "Invalid length: %d" len |> BitVectorException |> raise
    else initSmall len (num &&& (getMax len))

  let initBig len big = {
    Length = len
    Num = 0UL
    BigNum = big
  }

  let private initBigCheck len big = initBig len (big &&& (getBigMax len))

  let mkMax len =
    if len > 64 then initBig len (getBigMax len)
    else initSmall len (getMax len)

  let private mkBigSMax len = bigint.Pow (2I, len - 1) - bigint.One
  let private getBigSMax len = sMaxDict.GetOrAdd (len, mkBigSMax)
  let private getSmallSMax len = Array.get sMaxArr (len - 1)

  let private mkBigSMin len = bigint.Pow (2I, len)
  let private getBigSMin len = sMinDict.GetOrAdd (len, mkBigSMin)
  let private getSmallSMin len = Array.get sMinArr (len - 1)

  let private checkLength bv1 bv2 =
    if bv1.Length = bv2.Length then bv1.Length
    else sprintf "checkLength fail: %d != %d" bv1.Length bv2.Length
         |> BitVectorException |> raise

  let private negBigInt len num = bigint.Pow (2I, len) - num

  let private toSignedBig len big =
    if getBigSMax len >= big then big
    else big - (getBigPow len)

  let private toUnsignedBig len (big: bigint) =
    if big.Sign < 0 then big + (getBigPow len)
    else big

  let toInt bv =
    if bv.Length > 64 then int bv.BigNum
    else int bv.Num

  let mkZero size =
    if size > 64 then initBigCheck size bigint.Zero
    else initSmall size 0UL

  let getLength bv = bv.Length

  let getBigNum bv = bv.BigNum

  let getNum bv = bv.Num

  let getValue bv =
    if bv.Length <= 64 then bigint bv.Num
    else bv.BigNum

  let True = initSmall 1 1UL

  let False = initSmall 1 0UL

  let isZero bv =
    if bv.Length > 64 then bv.BigNum = bigint.Zero
    else bv.Num = 0UL

  let isOne bv =
    if bv.Length > 64 then bv.BigNum = bigint.One
    else bv.Num = 1UL

  let isSmallNeg bv = (bv.Num >>> bv.Length) = 1UL

  let isBigNeg bv = (bv.BigNum >>> bv.Length) = bigint.One

  let isAllTrue bv =
    let length = bv.Length
    if length <= 64 then bv.Num = getMax length
    else bv.BigNum = getBigMax length

  let isSMax bv =
    let length = bv.Length
    if length <= 64 then bv.Num = getSmallSMax length
    else bv.BigNum = getBigSMax length

  let isSMin bv =
    let length = bv.Length
    if length <= 64 then bv.Num = getSmallSMin length
    else bv.BigNum = getBigSMin length

  let ofInt (v: int) = initSmall 32 (uint64 v)

  let ofIntLen len (v: int) =
    if len <= 64 then initSmall len (uint64 v)
    else initBig len (bigint v)

  let ofInt64 (v: int64) = initSmall 64 (uint64 v)

  let private bytesToUInt64 bytes =
    let len = Array.length bytes
    let bytes = Array.create (8 - len) 0uy |> Array.append bytes
    BitConverter.ToUInt64 (bytes, 0)

  let ofBytes length (bytes: byte []) =
    match length with
    | 1 -> if Array.get bytes 0 = 0uy then False else True
    | 8 -> Array.get bytes 0 |> uint64 |> initSmall 8
    | 16 -> BitConverter.ToUInt16 (bytes, 0) |> uint64 |> initSmall 16
    | 32 -> BitConverter.ToUInt32 (bytes, 0) |> uint64 |> initSmall 32
    | 64 -> BitConverter.ToUInt64 (bytes, 0) |> initSmall 64
    | length when length <= 64 ->
      initSmall length ((bytesToUInt64 bytes) &&& (getMax length))
    | length -> initBigCheck length (bigint bytes)

  let private binOp op bigOp bv1 bv2 =
    match checkLength bv1 bv2 with
    | 1  -> { bv1 with Num = (op bv1.Num bv2.Num) &&& 1UL }
    | 8  -> { bv1 with Num = op bv1.Num bv2.Num |> uint8 |> uint64 }
    | 16 -> { bv1 with Num = op bv1.Num bv2.Num |> uint16 |> uint64 }
    | 32 -> { bv1 with Num = op bv1.Num bv2.Num |> uint32 |> uint64 }
    | 64 -> { bv1 with Num = op bv1.Num bv2.Num }
    | length when length <= 64 ->
      { bv1 with Num = (op bv1.Num bv2.Num) &&& getMax length }
    | length -> bigOp (bv1.BigNum, bv2.BigNum) |> initBigCheck length

  let private sBinOp op8 op16 op32 op64 bigOp bv1 bv2 =
    match checkLength bv1 bv2 with
    | 8 ->
      { bv1 with Num = op8 (int8 bv1.Num) (int8 bv2.Num) |> uint8 |> uint64 }
    | 16 ->
      { bv1 with Num = op16 (int16 bv1.Num) (int16 bv2.Num) |> uint16 |> uint64 }
    | 32 ->
      { bv1 with Num = op32 (int32 bv1.Num) (int32 bv2.Num) |> uint32 |> uint64 }
    | 64 -> { bv1 with Num = op64 bv1.Num bv2.Num |> uint64 }
    | length when length > 64 ->
      let big1 = toSignedBig length bv1.BigNum
      let big2 = toSignedBig length bv2.BigNum
      { bv1 with BigNum = bigOp (big1, big2) }
    | length -> sprintf "Invalid length: %d" length |> BitVectorException
                |> raise

  let mul bv1 bv2 = binOp (*) bigint.Multiply bv1 bv2

  let add bv1 bv2 = binOp (+) bigint.Add bv1 bv2

  let sub bv1 bv2 = binOp (-) bigint.Subtract bv1 bv2

  let udiv bv1 bv2 = binOp (/) bigint.Divide bv1 bv2

  let sdiv bv1 bv2 = sBinOp (/) (/) (/) (/) bigint.Divide bv1 bv2

  let urem bv1 bv2 = binOp (%) bigint.op_Modulus bv1 bv2

  let srem bv1 bv2 = sBinOp (%) (%) (%) (%) bigint.op_Modulus bv1 bv2

  let doAnd bv1 bv2 = binOp (&&&) bigint.op_BitwiseAnd bv1 bv2

  let doOr bv1 bv2 = binOp (|||) bigint.op_BitwiseOr bv1 bv2

  let xor bv1 bv2 = binOp (^^^) bigint.op_ExclusiveOr bv1 bv2

  let shl bv1 bv2 =
    let n = toInt bv2
    if n = 0 then bv1
    else
      let len = bv1.Length
      if len > 64 then initBigCheck len (bv1.BigNum <<< n)
      (* In .Net, 1UL <<< 64 = 1UL *)
      elif n >= 64 then initSmall len 0UL
      else initSmallCheck len (bv1.Num <<< n)

  let lShr bv1 bv2 =
    let n = toInt bv2
    if n = 0 then bv1
    else
      let len = bv1.Length
      if len > 64 then initBigCheck len (bv1.BigNum >>> n)
      else initSmallCheck len (bv1.Num >>> min n 63)

  let aShr bv1 bv2 =
    let n = toInt bv2
    if n = 0 then bv1
    else
      match bv1.Length with
      | 1  -> bv1
      | 8  -> { bv1 with Num = int8 bv1.Num >>> min n 7 |> uint8 |> uint64 }
      | 16 -> { bv1 with Num = int16 bv1.Num >>> min n 15 |> uint16 |> uint64 }
      | 32 -> { bv1 with Num = int32 bv1.Num >>> min n 31 |> uint32 |> uint64 }
      | 64 -> { bv1 with Num = int64 bv1.Num >>> min n 63 |> uint64 }
      | length when length > 64 ->
        let big = toSignedBig length bv1.BigNum
        { bv1 with BigNum = toUnsignedBig length (big >>> n) }
      | length -> sprintf "Invalid length: %d" length |> BitVectorException
                  |> raise

  let doNot bv =
    if bv = True then False
    elif bv = False then True
    else BitVectorException "doNot" |> raise

  let neg bv =
    let len = bv.Length
    if len > 64 then { bv with BigNum = negBigInt len bv.BigNum }
    else initSmallCheck len (uint64 (- (int64 bv.Num)))

  let private uRelOp op bigOp bv1 bv2 =
    let length = checkLength bv1 bv2
    if length > 64 then
      if bigOp (bv1.BigNum, bv2.BigNum) then True else False
    else
      if op bv1.Num bv2.Num then True else False

  let private sRelOp op8 op16 op32 op64 bigOp bv1 bv2 =
    match checkLength bv1 bv2 with
    | 1
    | 8  -> if op8 (int8 bv1.Num) (int8 bv2.Num) then True else False
    | 16 -> if op16 (int16 bv1.Num) (int16 bv2.Num) then True else False
    | 32 -> if op32 (int32 bv1.Num) (int32 bv2.Num) then  True else False
    | 64 -> if op64 bv1.Num bv2.Num then True else False
    | length when length > 64 ->
      let big1 = toSignedBig length bv1.BigNum
      let big2 = toSignedBig length bv2.BigNum
      if bigOp (big1, big2) then True else False
    | length -> sprintf "Invalid length: %d" length |> BitVectorException
                |> raise

  let ult bv1 bv2 = uRelOp (<) bigint.op_LessThan bv1 bv2

  let ule bv1 bv2 = uRelOp (<=) bigint.op_LessThanOrEqual bv1 bv2

  let ugt bv1 bv2 = ult bv2 bv1

  let uge bv1 bv2 = ule bv2 bv1

  let slt bv1 bv2 = sRelOp (<) (<) (<) (<) bigint.op_LessThan bv1 bv2

  let sle bv1 bv2 = sRelOp (<=) (<=) (<=) (<=) bigint.op_LessThanOrEqual bv1 bv2

  let sgt bv1 bv2 = slt bv2 bv1

  let sge bv1 bv2 = sle bv2 bv1

  let private take bv length =
    let bvLen = bv.Length
    if bvLen = length then bv
    elif bvLen > length then
      if bvLen > 64 then
        let big = bv.BigNum &&& getBigMax length
        if length > 64 then initBigCheck length big
        else initSmall length (uint64 big)
      else initSmall length (bv.Num &&& getMax length)
    else sprintf "take fail: %d < %d" bv.Length length
         |> BitVectorException |> raise

  let extract bv pos length =
    if isZero pos then take bv length
    else take (lShr bv pos) length

  let toString bv =
    if bv.Length > 64 then sprintf "%A<%d>" bv.BigNum bv.Length
    else sprintf "%u<%d>" bv.Num bv.Length

  let zext bv size =
    let length = getLength bv
    if length <= 64 && size <= 64 then { bv with Length = size }
    elif length <= 64 && size > 64 then initBig size (bigint bv.Num)
    else { bv with Length = size }

  let sext bv size =
    let length = getLength bv
    if length > size then
      sprintf "sext fail: length(%d) > size(%d)" length size
      |> BitVectorException |> raise
    elif length = size then bv
    elif size <= 64 then
      if isSmallNeg bv then
        initSmall size (((getMax (size - length)) <<< length) ||| bv.Num)
      else initSmall size bv.Num
    else
      if isBigNeg bv then
        let value = getValue bv
        initBig size (((getBigMax (size - length)) <<< length) ||| value)
      else initBig size bv.BigNum

  let concat bv1 bv2 =
    let l1 = getLength bv1
    let length = l1 + (getLength bv2)
    let value = getValue bv1 ||| (getValue bv2  <<< l1)
    if length <= 64 then initSmall length (uint64 value)
    else initBigCheck length value

  let replace dst pos src =
    let dl = getLength dst
    let sl = getLength src
    if sl + pos > dl then
      sprintf "replace fail: %d + %d > %d" sl pos dl |> BitVectorException
      |> raise
    else
      let dv = getValue dst
      let sv = getValue src
      let andKey = (getBigMax sl) <<< pos
      let dv = (sv <<< pos) ||| (andKey &&& dv)
      if dl <= 64 then initSmall dl (uint64 dv)
      else initBig dl dv

  let toHalf bv = failwith "todo"

  let toFloat bv =
    BitConverter.ToSingle (BitConverter.GetBytes (uint32 bv.Num), 0)

  let toDouble bv =
    BitConverter.ToDouble (BitConverter.GetBytes bv.Num, 0)

  let toFp128 bv = failwith "todo"
  let toX86Fp80 bv = failwith "todo"
  let toPpcFp128 bv = failwith "todo"

  let ofHalf v = failwith "todo"

  let ofFloat (v: float32) =
    BitConverter.ToUInt32 (BitConverter.GetBytes v, 0)
    |> uint64 |> initSmall 32

  let ofDouble (v: double) =
    BitConverter.ToUInt64 (BitConverter.GetBytes v, 0) |> initSmall 64

  let ofFp128 v = failwith "todo"
  let ofX86Fp80 v = failwith "todo"
  let ofPpcFp128 v = failwith "todo"

  let convFp bv srcTy dstTy =
    match srcTy, dstTy with
    | Half, Half
    | FloatTy.Float, FloatTy.Float
    | Double, Double
    | Fp128, Fp128
    | X86Fp80, X86Fp80
    | PpcFp128, PpcFp128 -> bv
    | _ -> failwith "todo"

  let fpToSI bv srcTy size =
    let convert big =
      if big <= getBigSMax size then
        if size <= 64 then initSmallCheck size (uint64 big) |> Some
        else initBigCheck size big |> Some
      else None

    match srcTy with
    | Half -> failwith "todo"
    | FloatTy.Float -> toFloat bv |> bigint
    | Double -> toDouble bv |> bigint
    | Fp128
    | X86Fp80
    | PpcFp128 -> failwith "todo"
    |> convert

  let fpToUI bv srcTy size = fpToSI bv srcTy size
  let uiToFp bv srcSize dstTy = failwith "todo"
  let siToFp bv srcSize dstTy = failwith "todo"

  let fcmpGT ty bv1 bv2 =
    match ty with
    | Half -> (toHalf bv1) < (toHalf bv2)
    | FloatTy.Float -> (toFloat bv1) < (toFloat bv2)
    | Double -> (toDouble bv1) < (toDouble bv2)
    | Fp128 -> (toFp128 bv1) < (toFp128 bv2)
    | X86Fp80
    | PpcFp128 -> failwith "todo"

  let fBinOp ty op1 op2 op3 op4 bv1 bv2 =
    match ty with
    | Half -> op1 (toHalf bv1) (toHalf bv2) |> ofHalf
    | FloatTy.Float -> op2 (toFloat bv1) (toFloat bv2) |> ofFloat
    | Double -> op3 (toDouble bv1) (toDouble bv2) |> ofDouble
    | Fp128 -> op4 (toFp128 bv1) (toFp128 bv2) |> ofFp128
    | X86Fp80
    | PpcFp128 -> failwith "todo"

  let fadd ty bv1 bv2 = fBinOp ty (+) (+) (+) (+) bv1 bv2
  let fsub ty bv1 bv2 = fBinOp ty (-) (-) (-) (-) bv1 bv2
  let fmul ty bv1 bv2 = fBinOp ty (*) (*) (*) (*) bv1 bv2
  let fdiv ty bv1 bv2 = fBinOp ty (/) (/) (/) (/) bv1 bv2
  let frem ty bv1 bv2 = fBinOp ty (%) (%) (%) (%) bv1 bv2

  let convertToString bv =
    let bytes =
      if bv.Length > 64 then bv.BigNum.ToByteArray()
      else BitConverter.GetBytes (bv.Num)
    (System.Text.Encoding.ASCII.GetString (bytes)).Trim('\x00')
