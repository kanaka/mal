module Eval

    open Types

    type Env = Map<string, Node>

    let errExpected tok = EvalError(sprintf "expected %s" tok)

    let iterPairs f (source : seq<_>) =
        use iter = source.GetEnumerator()
        let rec loop () =
            if iter.MoveNext() then
                let first = iter.Current
                if not (iter.MoveNext()) then raise <| errExpected "even node count"
                let second = iter.Current
                f first second
                loop ()
        loop ()

    let rec eval_ast env = function
        | Symbol(sym) -> Env.get env sym
        | List(lst) -> lst |> List.map (eval env) |> List
        | Vector(arr) -> arr |> Array.map (eval env) |> Vector
        | Map(map) -> map |> Map.map (fun k v -> eval env v) |> Map
        | node -> node

    and defBangForm env = function
        | [sym; form] ->
            match sym with
            | Symbol(sym) ->
                let node = eval env form
                Env.set env sym node
                node
            | _ -> raise <| errExpected "symbol"
        | _ -> raise <| Core.errArity ()

    and setBinding env first second =
        let s = match first with 
                | Symbol(s) -> s 
                | _ -> raise <| errExpected "symbol"
        let form = eval env second
        Env.set env s form

    and letStarForm env = function
        | [bindings; form] ->
            let newEnv = Env.makeNew env [] []
            let binder = setBinding newEnv
            match bindings with
            | List(lst) -> lst |> iterPairs binder
            | Vector(vec) -> vec |> iterPairs binder
            | _ -> raise <| errExpected "list or vector"
            eval newEnv form
        | _ -> raise <| Core.errArity ()

    and ifForm env = function
        | [condForm; trueForm; falseForm] -> ifForm3 env condForm trueForm falseForm
        | [condForm; trueForm] -> ifForm3 env condForm trueForm Nil
        | _ -> raise <| Core.errArity ()

    and ifForm3 env condForm trueForm falseForm =
        match eval env condForm with
        | Bool(false) | Nil -> eval env falseForm
        | _ -> eval env trueForm

    and doForm env = function
        | [a] -> eval env a
        | a::rest ->
            eval env a |> ignore
            doForm env rest
        | _ -> raise <| Core.errArity ()

    and fnStarForm outer nodes =
        let makeFunc binds body =
            Env.makeFunc (fun nodes ->
                let inner = Env.makeNew outer binds nodes
                eval inner body)

        match nodes with
        | [List(binds); body] -> makeFunc binds body
        | [Vector(binds); body] -> makeFunc (List.ofArray binds) body
        | [_; _] -> raise <| errExpected "bindings of list or vector"
        | _ -> raise <| Core.errArity ()

    and eval env = function
        | List(Symbol("def!")::rest) -> defBangForm env rest
        | List(Symbol("let*")::rest) -> letStarForm env rest
        | List(Symbol("if")::rest) -> ifForm env rest
        | List(Symbol("do")::rest) -> doForm env rest
        | List(Symbol("fn*")::rest) -> fnStarForm env rest
        | List(_) as node ->
            let resolved = node |> eval_ast env
            match resolved with
            | List(Func(_, f)::rest) -> f rest
            | _ -> raise <| errExpected "function"
        | node -> node |> eval_ast env
