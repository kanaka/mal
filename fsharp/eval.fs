module Eval

    open Types

    type Env = Map<string, Node>

    let errExpected tok = EvalError(sprintf "expected %s" tok)

    let mapPairs f (source : seq<_>) =
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

    and defBang env = function
        | sym::node::[] -> 
            match sym with
            | Symbol(sym) -> 
                let node = eval env node 
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

    and letStar env = function
        | bindings::form::[] ->
            let newEnv = Env.makeNew env
            let binder = setBinding newEnv
            match bindings with
            | List(lst) -> lst |> mapPairs binder
            | Vector(vec) -> vec |> mapPairs binder
            | _ -> raise <| errExpected "list or vector"
            eval newEnv form
        | _ -> raise <| Core.errArity ()

    and eval env = function
        | List(Symbol("def!")::rest) -> defBang env rest
        | List(Symbol("let*")::rest) -> letStar env rest
        | List(_) as node ->
            let resolved = node |> eval_ast env
            match resolved with
            | List(Func({F = f})::rest) -> f rest
            | _ -> raise <| errExpected "function"
        | node -> node |> eval_ast env
