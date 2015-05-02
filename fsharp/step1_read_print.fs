module REPL
    open System

    let read input =
        try
            Reader.read_str input
        with
        | Error.ReaderError(msg) ->
            printfn "%s" msg
            []

    let eval ast =
        Some(ast)

    let print v =
        v
        |> Seq.singleton
        |> Printer.pr_str
        |> printfn "%s"

    let rep input =
        read input
        |> Seq.ofList
        |> Seq.map (fun form -> eval form)
        |> Seq.filter Option.isSome
        |> Seq.iter (fun value -> print value.Value)

    let getReadlineMode (args : string array) =
        if args.Length > 0 && args.[0] = "--raw" then
            Readline.Mode.Raw
        else
            Readline.Mode.Terminal

    [<EntryPoint>]
    let rec main args =
        let mode = getReadlineMode args
        match Readline.read "user> " mode with
        | null -> 0
        | input -> 
            rep input
            main args
