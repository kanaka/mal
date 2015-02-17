module Eval

    open Types

    type Env = Map<string, Node>

    let errExpected tok = EvalError(sprintf "expected %s" tok)

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

    and setListBinding env = function
        | sym::form::rest ->
            let s = match sym with | Symbol(s) -> s | _ -> raise <| errExpected "symbol"
            let form = eval env form
            Env.set env s form
            setListBinding env rest
        | [] -> ()
        | _ -> raise <| errExpected "even nodes"

    and setVectorBinding env (nodes : Node array) =
        if nodes.Length % 2 = 1 then raise <| errExpected "even nodes"
        let rec loop pos =
            if pos < nodes.Length then
                let s = match nodes.[pos] with 
                        | Symbol(s) -> s 
                        | _ -> raise <| errExpected "symbol"
                let form = eval env <| nodes.[pos + 1]
                Env.set env s form
                loop (pos + 2)
            else
                ()
        loop 0

    and letStar env = function
        | bindings::form::[] ->
            let newEnv = Env.makeNew env
            match bindings with
            | List(lst) -> setListBinding newEnv lst
            | Vector(vec) -> setVectorBinding newEnv vec
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
