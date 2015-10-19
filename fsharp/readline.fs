module Readline
    open System
    open Mono.Terminal

    type Mode =
    | Terminal
    | Raw

    let read prompt = function
    | Terminal 
        -> let editor = LineEditor("Mal")
           editor.Edit(prompt, "")
    | Raw
        -> Console.Write(prompt)
           Console.Out.Flush()
           Console.ReadLine() 
