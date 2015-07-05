module REPL
    open System
    open Node
    open Types

    let rec iterPairs f = function
        | Pair(first, second, t) ->
            f first second
            iterPairs f t
        | Empty -> ()
        | _ -> raise <| Error.errExpectedX "list or vector"

    let rec eval_ast env = function
        | Symbol(sym) -> Env.get env sym
        | List(_, lst) -> lst |> List.map (eval env) |> makeList
        | Vector(_, seg) -> seg |> Seq.map (eval env) |> Array.ofSeq |> Node.ofArray
        | Map(_, map) -> map |> Map.map (fun k v -> eval env v) |> makeMap
        | node -> node

    and defBang env = function
        | [sym; node] ->
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
        | [bindings; form] ->
            let newEnv = Env.makeNew env [] []
            let binder = setBinding newEnv
            match bindings with
            | List(_, _) | Vector(_, _) -> iterPairs binder bindings
            | _ -> raise <| Error.errExpectedX "list or vector"
            eval newEnv form
        | _ -> raise <| Error.wrongArity ()

    and eval env = function
        | List(_, Symbol("def!")::rest) -> defBang env rest
        | List(_, Symbol("let*")::rest) -> letStar env rest
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
