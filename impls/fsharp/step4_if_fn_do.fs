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

    let rec defBangForm env = function
        | [sym; form] ->
            match sym with
            | Symbol(sym) ->
                let node = eval env form
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

    and letStarForm env = function
        | [bindings; form] ->
            let newEnv = Env.makeNew env [] []
            let binder = setBinding newEnv
            match bindings with
            | List(_, _) | Vector(_, _) -> iterPairs binder bindings
            | _ -> raise <| Error.errExpectedX "list or vector"
            eval newEnv form
        | _ -> raise <| Error.wrongArity ()

    and ifForm env = function
        | [condForm; trueForm; falseForm] -> ifForm3 env condForm trueForm falseForm
        | [condForm; trueForm] -> ifForm3 env condForm trueForm Nil
        | _ -> raise <| Error.wrongArity ()

    and ifForm3 env condForm trueForm falseForm =
        match eval env condForm with
        | Bool(false) | Nil -> eval env falseForm
        | _ -> eval env trueForm

    and doForm env = function
        | [a] -> eval env a
        | a::rest ->
            eval env a |> ignore
            doForm env rest
        | _ -> raise <| Error.wrongArity ()

    and fnStarForm outer nodes =
        let makeFunc binds body =
            let f = fun nodes ->
                        let inner = Env.makeNew outer binds nodes
                        eval inner body
            Env.makeFunc f body binds outer

        match nodes with
        | [List(_, binds); body] -> makeFunc binds body
        | [Vector(_, seg); body] -> makeFunc (List.ofSeq seg) body
        | [_; _] -> raise <| Error.errExpectedX "bindings of list or vector"
        | _ -> raise <| Error.wrongArity ()

    and eval env ast =
        ignore <| match Env.get env "DEBUG-EVAL" with
                  | None | Some(Bool(false)) | Some(Nil) -> ()
                  | _ -> Printer.pr_str [ast] |> printfn "EVAL: %s"
        match ast with
        | Symbol(sym) -> match Env.get env sym with
                         | Some(value) -> value
                         | None -> Error.symbolNotFound sym |> raise
        | Vector(_, seg) -> seg |> Seq.map (eval env) |> Array.ofSeq |> Node.ofArray
        | Map(_, map) -> map |> Map.map (fun k v -> eval env v) |> makeMap
        | List(_, Symbol("def!")::rest) -> defBangForm env rest
        | List(_, Symbol("let*")::rest) -> letStarForm env rest
        | List(_, Symbol("if")::rest) -> ifForm env rest
        | List(_, Symbol("do")::rest) -> doForm env rest
        | List(_, Symbol("fn*")::rest) -> fnStarForm env rest
        | List(_, (a0 :: rest)) ->
            let args = List.map (eval env) rest
            match eval env a0 with
            | BuiltInFunc(_, _, f) -> f args
            | Func(_, _, _, body, binds, outer) ->
                let inner = Env.makeNew outer binds args
                body |> eval inner
            | _ -> raise <| Error.errExpectedX "func"
        | _ -> ast

    let READ input =
        Reader.read_str input

    let EVAL env ast =
        Some(eval env ast)

    let PRINT v =
        v
        |> Seq.singleton
        |> Printer.pr_str
        |> printfn "%s"

    let RE env input =
        READ input
        |> Seq.ofList
        |> Seq.choose (fun form -> EVAL env form)

    let REP env input =
        input
        |> RE env
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

        RE env "(def! not (fn* (a) (if a false true)))" |> Seq.iter ignore

        let rec loop () =
            match Readline.read "user> " mode with
            | null -> 0
            | input ->
                try
                    REP env input
                with
                | Error.EvalError(str)
                | Error.ReaderError(str) ->
                    printfn "Error: %s" str
                | ex ->
                    printfn "Error: %s" (ex.Message)
                loop ()
        loop ()
