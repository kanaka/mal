module Eval

    open Types

    type Env = Map<string, Node>

    let errFuncExpected () = EvalError("expected function")
    let errNodeExpected () = EvalError("expected node")
    let errSymbolExpected () = EvalError("expected symbol")

    let rec eval_ast env = function
        | Symbol(sym) -> Env.get env sym
        | List(lst) -> lst |> List.map (eval env) |> List
        | Vector(arr) -> arr |> Array.map (eval env) |> Vector
        | Map(map) -> map |> Map.map (fun k v -> eval env v) |> Map
        | node -> node

    and def env = function
        | symb::node::[] -> 
            match symb with
            | Symbol(sym) -> 
                let node = eval env node 
                Env.set env sym node
                node
            | _ -> raise <| errSymbolExpected ()
        | _ -> raise <| Core.errArity () 

    and eval env = function
        | List(Symbol("def!")::rest) -> def env rest
        | List(_) as node ->
            let resolved = node |> eval_ast env
            match resolved with
            | List(Func({F = f})::rest) -> f rest
            | _ -> raise <| errFuncExpected ()
        | node -> node |> eval_ast env
