module REPL
    open System

    let read input =
        try
            Reader.read_str input
        with
        | Types.ReaderError(msg) ->
            printfn "%s" msg
            []

    let eval env ast =
        try
            Some(Eval.eval env ast)
        with
        | Types.EvalError(msg) ->
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

    let getReadlineMode args =
        if args |> Array.exists (fun e -> e = "--raw") then
            Readline.Mode.Raw
        else
            Readline.Mode.Terminal

    let eval_func env = function
        | [ast] -> Eval.eval env ast
        | _ -> raise <| Core.errArity ()

    let argv_func = function
        | file::rest -> rest |> List.map Types.String |> Types.List
        | [] -> Types.List([])

    let configureEnv args =
        let env = Env.makeRootEnv ()

        Env.set env "eval" <| Env.makeBuiltInFunc (fun nodes -> eval_func env nodes)
        Env.set env "*ARGV*" <| argv_func args

        re env """
            (def! not (fn* (a) (if a false true)))
            (def! load-file (fn* (f) (eval (read-string (slurp f)))))
            """ |> Seq.iter ignore

        env

    [<EntryPoint>]
    let main args =
        let mode = getReadlineMode args
        let args = Seq.ofArray args |> Seq.filter (fun e -> e <> "--raw") |> List.ofSeq
        let env = configureEnv args

        match args with
        | file::_ ->
            System.IO.File.ReadAllText file
            |> rep env
            0
        | _ ->
            let rec loop () =
                match Readline.read "user> " mode with
                | null -> 0
                | input ->
                    rep env input
                    loop ()
            loop ()
