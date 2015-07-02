module REPL
    let READ input =
        input

    let EVAL ast =
        ast

    let PRINT v =
        printfn "%s" v

    let REP input =
        input
        |> READ
        |> EVAL
        |> PRINT

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
            REP input
            main args
