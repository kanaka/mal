module REPL
    open System

    let READ input =
        try
            Reader.read_str input
        with
        | Error.ReaderError(msg) ->
            printfn "%s" msg
            []

    let EVAL ast =
        Some(ast)

    let PRINT v =
        v
        |> Seq.singleton
        |> Printer.pr_str
        |> printfn "%s"

    let REP input =
        READ input
        |> Seq.ofList
        |> Seq.map (fun form -> EVAL form)
        |> Seq.filter Option.isSome
        |> Seq.iter (fun value -> PRINT value.Value)

    let getReadlineMode args =
        if args |> Array.exists (fun e -> e = "--raw") then
            Readline.Mode.Raw
        else
            Readline.Mode.Terminal

    [<EntryPoint>]
    let rec main args =
        let mode = getReadlineMode args
        match Readline.read "user> " mode with
        | null -> 0
        | input -> 
            REP input
            main args
