# FsSymExe
`FsSymExe` is a static symbolic execution engine based on LLVM IR and written in [F#](https://fsharp.org).
This is used as a configurable symbolic execution engine in our paper, "Precise and Scalable Detection of Use-after-Compacting-Garbage-Collection Bugs", which appeared in Usenix Security 2021.
We expect that this can be leveraged to detect other kinds of vulnerabilities or write another analysis.

## Installation
1. Install `dotnet`

Installation of `dotnet` depends on OS, so please check this [link](https://dotnet.microsoft.com/download/linux-package-manager/ubuntu18-04/sdk-current).


2. Clone and build `FsSymExe`
```
$ git clone git@github.com:DaramG/FsSymExe.git
$ cd FsSymExe
$ make
```

## Usage
We can use FsSymExe by setting configurations such as callbacks for specific operations and an user configurable state.
You can find a list of configurations in [here](src/SymExecutor/State.fs).
This is a part of the symbolic state definitions and represents user configurable settings.

```fsharp
type State<'T> =
  ...
  UserState: 'T
  IsReachable: State<'T> -> BlockID -> int -> State<'T> option
  GetSymFunc: State<'T> -> ID -> SymFunc_<'T> option
  StmtCb: State<'T> -> int -> State<'T>
  CallCb: State<'T> -> int -> ID -> State<'T>
  LoadCb: State<'T> -> Expr -> ExprSize -> Expr -> State<'T>
  StoreCb: State<'T> -> Expr -> Expr -> State<'T>
  ReturnCb: State<'T> -> Expr option -> State<'T>
  UseSAT: bool
```

Here is an example code that installs user configurable settings.

```fsharp
let check conf prog =
  let state =
    State.init prog GcState.empty
    |> State.setUseSAT false
    |> State.setIsReachable isReachable
    |> State.setLoadCb loadCb
    |> State.setStoreCb storeCb
    |> State.setReturnCb returnCb
```

You can get the full code for this example in this [project](https://github.com/DaramG/CGSan).

## Projects
`FsSymExe` is used in these proejcts:
- [CGSan](https://github.com/DaramG/CGSan), Precise and Scalable Detection of Use-after-Compacting-Garbage-Collection Bugs (Usenix Security 2021) ([pdf](https://www.usenix.org/system/files/sec21-han-hyungseok.pdf))
