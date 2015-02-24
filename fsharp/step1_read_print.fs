module REPL
    open System

    let read input =
        Reader.read_str input

    let eval ast =
        ast

    let print v =
        v
        |> Printer.pr_str
        |> Console.WriteLine

    let rep input =
        input
        |> read
        |> eval
        |> print

    [<EntryPoint>]
    let rec main args =
        let mode = if args.Length > 0 && args.[0] = "--raw" then Readline.Mode.Raw else Readline.Mode.Terminal
        match Readline.read "user> " mode with
        | null -> 0
        | input -> 
            rep input
            main args
