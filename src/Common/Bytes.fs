module Common.Bytes

open System

let toInt32 bytes = BitConverter.ToInt32 (bytes, 0)

let toUInt64 bytes = BitConverter.ToUInt64 (bytes, 0)
