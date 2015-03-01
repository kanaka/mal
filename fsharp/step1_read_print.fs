module REPL
    open System

    type OptionBuilder() =
        member x.Bind(v, f) = Option.bind f v
        member x.Return v = Some v
        member x.ReturnFrom o = o
        member x.Zero() = Some ()

    let read input =
        try
            Reader.read_str input
        with
        | Reader.ReaderError(msg) ->
            msg |> printfn "%s"
            None

    let eval ast =
        Some(ast)

    let print v =
        v
        |> Printer.pr_str
        |> printfn "%s"

    let rep input =
        OptionBuilder() {
            let! ast = read input
            let! value = eval ast
            print value
        } |> ignore

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
