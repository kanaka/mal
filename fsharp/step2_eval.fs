module REPL
    open System
    open Node
    open Types

    let rec eval_ast env = function
        | Symbol(sym) -> Env.get env sym
        | List(_, lst) -> lst |> List.map (eval env) |> makeList
        | Vector(_, seg) -> seg |> Seq.map (eval env) |> Array.ofSeq |> Node.ofArray
        | Map(_, map) -> map |> Map.map (fun k v -> eval env v) |> makeMap
        | node -> node

    and eval env = function
        | List(_, _) as node ->
            let resolved = node |> eval_ast env
            match resolved with
            | List(_, BuiltInFunc(_, _, f)::rest) -> f rest
            | _ -> raise <| Error.errExpectedX "func"
        | node -> node |> eval_ast env

    let READ input =
        try
            Reader.read_str input
        with
        | Error.ReaderError(msg) ->
            printfn "%s" msg
            []

    let EVAL env ast =
        try
            Some(eval env ast)
        with
        | Error.EvalError(msg) 
        | Error.ReaderError(msg) ->
            printfn "%s" msg
            None

    let PRINT v =
        v
        |> Seq.singleton
        |> Printer.pr_str
        |> printfn "%s"

    let REP env input =
        READ input
        |> Seq.ofList
        |> Seq.choose (fun form -> EVAL env form)
        |> Seq.iter (fun value -> PRINT value)

    let getReadlineMode args =
        if args |> Array.exists (fun e -> e = "--raw") then
            Readline.Mode.Raw
        else
            Readline.Mode.Terminal

    [<EntryPoint>]
    let rec main args =
        let mode = getReadlineMode args
        let env = Env.makeRootEnv ()
        match Readline.read "user> " mode with
        | null -> 0
        | input -> 
            REP env input
            main args
