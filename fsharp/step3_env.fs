module REPL
    open System
    open Node
    open Types

    let rec iterPairs f = function
        | Pair(first, second, t) ->
            f first second
            iterPairs f t
        | Empty -> ()
        | _ -> raise <| Error.expectedX "list or vector"

    let rec eval_ast env = function
        | Symbol(sym) -> Env.get env sym
        | List(lst) -> lst |> List.map (eval env) |> List
        | Vector(seg) -> seg |> Seq.map (eval env) |> Array.ofSeq |> Node.ofArray
        | Map(map) -> map |> Map.map (fun k v -> eval env v) |> Map
        | node -> node

    and defBang env = function
        | sym::node::[] ->
            match sym with
            | Symbol(sym) ->
                let node = eval env node
                Env.set env sym node
                node
            | _ -> raise <| Error.errExpectedX "symbol"
        | _ -> raise <| Error.wrongArity ()

    and setBinding env first second =
        let s = match first with
                | Symbol(s) -> s
                | _ -> raise <| Error.errExpectedX "symbol"
        let form = eval env second
        Env.set env s form

    and letStar env = function
        | bindings::form::[] ->
            let newEnv = Env.makeNew env [] []
            let binder = setBinding newEnv
            match bindings with
            | List(_) | Vector(_) -> iterPairs binder bindings
            | _ -> raise <| Error.errExpectedX "list or vector"
            eval newEnv form
        | _ -> raise <| Error.wrongArity ()

    and eval env = function
        | List(Symbol("def!")::rest) -> defBang env rest
        | List(Symbol("let*")::rest) -> letStar env rest
        | List(_) as node ->
            let resolved = node |> eval_ast env
            match resolved with
            | List(Func(_, f, _, _, [])::rest) -> f rest
            | _ -> raise <| Error.errExpectedX "function"
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

    let getReadlineMode (args : string array) =
        if args.Length > 0 && args.[0] = "--raw" then
            Readline.Mode.Raw
        else
            Readline.Mode.Terminal

    [<EntryPoint>]
    let main args =
        let mode = getReadlineMode args
        let env = Env.makeRootEnv ()
        let rec loop () =
            match Readline.read "user> " mode with
            | null -> 0
            | input ->
                REP env input
                loop ()
        loop ()
