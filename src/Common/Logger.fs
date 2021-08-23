namespace Common

open System
open System.Threading

module Logger =
  let mkFmt color txt = sprintf "\x1b[0;%dm%s\x1b[0m" color txt

  let mkTimeFmt str =
    System.DateTime.UtcNow.ToString("MM-dd:HH:mm:ss")
    |> sprintf "[%s, %s]" str

  let infoFmt () = mkTimeFmt "INFO" |> mkFmt 32
  let warnFmt () = mkTimeFmt "WARN" |> mkFmt 33
  let errorFmt () = mkTimeFmt "ERROR" |> mkFmt 31

  let private lock = ref (new Object ())

  let private acquire () =
    try Monitor.Enter (lock)
    finally ()

  let private release () = Monitor.Exit (lock)

  let printMsg (fmt: string) (msg: string) =
    acquire ()
    System.Console.WriteLine (fmt + " " + msg)
    release ()

  let printErrMsg (fmt: string) (msg: string) =
    acquire ()
    System.Console.Error.WriteLine (fmt + " " + msg)
    release ()

  let printMsgExit fmt msg =
    printErrMsg fmt msg
    exit 1

  let info fmt = Printf.kprintf (infoFmt () |> printMsg) fmt

  let warn fmt = Printf.kprintf (warnFmt () |> printErrMsg) fmt

  let errorNoExit fmt = Printf.kprintf (errorFmt () |> printErrMsg) fmt

  let error fmt = Printf.kprintf (errorFmt () |> printMsgExit) fmt
