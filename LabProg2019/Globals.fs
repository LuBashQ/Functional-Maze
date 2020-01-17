﻿(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Main.fs: main code
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

[< AutoOpen >]
module LabProg2019.Globals

open System
open System.Threading
open Printf
open System.IO.Pipes
open System.IO
open System.Diagnostics

/// Logger type.
type logger () =
    /// Internal low-level atomic print primitive.
    abstract atomic_print : Color -> string -> unit
    default this.atomic_print col msg =
        lock this <| fun () ->
            Console.ForegroundColor <- col
            Console.WriteLine msg
            Console.ResetColor ()
    
    member private this.print col msg =
        let th = Thread.CurrentThread
        let name = th.Name
        this.atomic_print col <| sprintf "[%s:%d%s] %s" name th.ManagedThreadId (if th.IsThreadPoolThread then "P" else "") msg

    member this.debug fmt =
        #if DEBUG
        kprintf (this.print Config.log_debug_color) fmt
        #else
        kprintf (fun _ -> ()) fmt
        #endif
    member this.msg fmt = kprintf (this.print Config.log_msg_color) fmt
    member this.warn fmt = kprintf (this.print Config.log_warn_color) fmt
    member this.error fmt = kprintf (this.print Config.log_error_color) fmt

/// Type specialization for the log server.
type remote_logger (client : NamedPipeClientStream) =
    inherit logger ()
    let w = new StreamWriter (client)

    override this.atomic_print col msg =
        lock this <| fun () ->
            w.WriteLine (col.ToString ())
            let s = msg.Replace ('\n', Config.log_pipe_translate_eol)
            w.WriteLine s
            w.Flush ()
            #if ECHO_LOG_ON_CLIENT
            base.prompt col msg
            #endif

/// Global logger instance.    
let mutable Log : logger = new logger ()
/// Global PRNG instance.
let Rnd = new Random ()


/// Generate a random integer within the range [a, b].
let rnd_int a b = Rnd.Next (a, b + 1)
/// Generate a random float within the range [a, b].
let rnd_float a b = Rnd.NextDouble () * (b - a) + a
/// Generate a random boolean.
let rnd_bool () = Rnd.Next (0, 2) = 0
/// Generate a random ConsoleColor among the available system palette.
let rnd_color () = let a = Enum.GetValues typeof<Color> in a.GetValue (rnd_int 0 (a.Length - 1)) :?> Color

let stopwatch_quiet f =
    let w = Stopwatch.StartNew ()
    f ()
    w.Stop ()
    w.Elapsed

let stopwatch msg f =
    let e = stopwatch_quiet f
    Log.debug "%s: %O ms" msg e.TotalMilliseconds
    e

let checkMatrixBounds (x: int, y: int, W: int, H: int) =
    x >= 0 && x < W && y >= 0 && y < H

let rec removeFromList (spr: 'a) (list: 'a list) = 
    match list with
    | [] -> []
    | x::xs -> if x = spr then removeFromList spr xs else x::removeFromList spr xs

let toArray (grid:'T[,]) = grid |> Seq.cast<'T> |> Seq.toArray



