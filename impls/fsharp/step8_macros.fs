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

    let rec qqLoop elt acc =
        match elt with
        | List(_, [Symbol("splice-unquote");list]) -> makeList [Symbol "concat"; list; acc]
        | List(_,  Symbol("splice-unquote")::_)    -> raise <| Error.wrongArity ()
        | _ -> makeList [Symbol "cons"; quasiquote elt; acc]
    and quasiquote = function
        | List(_, [Symbol("unquote");form]) -> form
        | List(_,  Symbol("unquote")::_)    -> raise <| Error.wrongArity ()
        | List (_, list) -> List.foldBack qqLoop list Node.EmptyLIST
        | Vector(_, segment) ->
            let array = Array.sub segment.Array segment.Offset segment.Count
            let folded = Array.foldBack qqLoop array Node.EmptyLIST
            makeList [Symbol "vec"; folded]
        | Map(_)    as ast -> makeList [Symbol "quote"; ast]
        | Symbol(_) as ast -> makeList [Symbol "quote"; ast]
        | ast -> ast

    let quoteForm = function
        | [node] -> node
        | _ -> raise <| Error.wrongArity ()

    let rec macroExpand env = function
        | Env.IsMacro env (Macro(_, _, f, _, _, _), rest) ->
            f rest |> macroExpand env
        | node -> node

    let rec eval_ast env = function
        | Symbol(sym) -> Env.get env sym
        | List(_, lst) -> lst |> List.map (eval env) |> makeList
        | Vector(_, seg) -> seg |> Seq.map (eval env) |> Array.ofSeq |> Node.ofArray
        | Map(_, map) -> map |> Map.map (fun k v -> eval env v) |> makeMap
        | node -> node

    and defBangForm env = function
        | [sym; form] ->
            match sym with
            | Symbol(sym) ->
                let node = eval env form
                Env.set env sym node
                node
            | _ -> raise <| Error.errExpectedX "symbol"
        | _ -> raise <| Error.wrongArity ()

    and defMacroForm env = function
        | [sym; form] ->
            match sym with
            | Symbol(sym) ->
                let node = eval env form
                match node with
                | Func(_, _, f, body, binds, outer) ->
                    let node = Env.makeMacro f body binds outer
                    Env.set env sym node
                    node
                | _ -> raise <| Error.errExpectedX "user defined func"
            | _ -> raise <| Error.errExpectedX "symbol"
        | _ -> raise <| Error.wrongArity ()

    and macroExpandForm env = function
        | [form] -> macroExpand env form
        | _ -> raise <| Error.wrongArity ()

    and setBinding env first second =
        let s = match first with
                | Symbol(s) -> s
                | _ -> raise <| Error.errExpectedX "symbol"
        let form = eval env second
        Env.set env s form

    and letStarForm outer = function
        | [bindings; form] ->
            let inner = Env.makeNew outer [] []
            let binder = setBinding inner
            match bindings with
            | List(_) | Vector(_) -> iterPairs binder bindings
            | _ -> raise <| Error.errExpectedX "list or vector"
            inner, form
        | _ -> raise <| Error.wrongArity ()

    and ifForm env = function
        | [condForm; trueForm; falseForm] -> ifForm3 env condForm trueForm falseForm
        | [condForm; trueForm] -> ifForm3 env condForm trueForm Nil
        | _ -> raise <| Error.wrongArity ()

    and ifForm3 env condForm trueForm falseForm =
        match eval env condForm with
        | Bool(false) | Nil -> falseForm
        | _ -> trueForm

    and doForm env = function
        | [a] -> a
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

    and eval env = function
        | List(_, _) as node ->
            match macroExpand env node with
            | List(_, []) as emptyList -> emptyList
            | List(_, Symbol("def!")::rest) -> defBangForm env rest
            | List(_, Symbol("defmacro!")::rest) -> defMacroForm env rest
            | List(_, Symbol("macroexpand")::rest) -> macroExpandForm env rest
            | List(_, Symbol("let*")::rest) ->
                let inner, form = letStarForm env rest
                form |> eval inner
            | List(_, Symbol("if")::rest) -> ifForm env rest |> eval env
            | List(_, Symbol("do")::rest) -> doForm env rest |> eval env
            | List(_, Symbol("fn*")::rest) -> fnStarForm env rest
            | List(_, Symbol("quote")::rest) -> quoteForm rest
            | List(_, [Symbol("quasiquoteexpand");form]) -> quasiquote form
            | List(_,  Symbol("quasiquoteexpand")::_)    -> raise <| Error.wrongArity ()
            | List(_, [Symbol("quasiquote");form]) -> eval env <| quasiquote form
            | List(_,  Symbol("quasiquote")::_)    -> raise <| Error.wrongArity ()
            | List(_, _) as node ->
                let resolved = node |> eval_ast env
                match resolved with
                | List(_, BuiltInFunc(_, _, f)::rest) -> f rest
                | List(_, Func(_, _, _, body, binds, outer)::rest) ->
                    let inner = Env.makeNew outer binds rest
                    body |> eval inner
                | _ -> raise <| Error.errExpectedX "func"
            | node -> node |> eval_ast env
        | node -> node |> eval_ast env

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

    let eval_func env = function
        | [ast] -> eval env ast
        | _ -> raise <| Error.wrongArity ()

    let argv_func = function
        | file::rest -> rest |> List.map Types.String |> makeList
        | [] -> EmptyLIST

    let configureEnv args =
        let env = Env.makeRootEnv ()

        Env.set env "eval" <| Env.makeBuiltInFunc (fun nodes -> eval_func env nodes)
        Env.set env "*ARGV*" <| argv_func args

        RE env """
            (def! not (fn* (a) (if a false true)))
            (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
            (defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))
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
            |> RE env |> Seq.iter ignore
            0
        | _ ->
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
