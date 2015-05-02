module REPL
    open System

    let read input =
        try
            Reader.read_str input
        with
        | Error.ReaderError(msg) ->
            printfn "%s" msg
            []

    let eval env ast =
        try
            Some(Eval.eval env ast)
        with
        | Error.EvalError(msg)
        | Error.ReaderError(msg) ->
            printfn "%s" msg
            None

    let print v =
        v
        |> Seq.singleton
        |> Printer.pr_str
        |> printfn "%s"

    let re env input =
        read input
        |> Seq.ofList
        |> Seq.choose (fun form -> eval env form)

    let rep env input =
        input
        |> re env
        |> Seq.iter (fun value -> print value)

    let getReadlineMode (args : string array) =
        if args.Length > 0 && args.[0] = "--raw" then
            Readline.Mode.Raw
        else
            Readline.Mode.Terminal

    [<EntryPoint>]
    let main args =
        let mode = getReadlineMode args
        let env = Env.makeRootEnv ()

        re env "(def! not (fn* (a) (if a false true)))" |> Seq.iter ignore

        let rec loop () =
            match Readline.read "user> " mode with
            | null -> 0
            | input ->
                rep env input
                loop ()
        loop ()
