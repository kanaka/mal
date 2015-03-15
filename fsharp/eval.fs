module Eval

    open Types

    type Env = Map<string, Node>

    let errFuncExpected () = EvalError("expected function")
    let errNotFound s = EvalError(sprintf "'%s' not found" s)
    
    let wrap tag name func =
        name, Func({ Tag = tag; Name = name; F = func })

    let makeEnv () =
        [ wrap 1 "+" Core.add;
          wrap 2 "-" Core.subtract;
          wrap 3 "*" Core.multiply;
          wrap 4 "/" Core.divide ]
        |> Map.ofList

    let lookup (env : Env) sym =
        match env.TryFind sym with
        | Some(f) -> f
        | None -> raise <| errNotFound sym

    let rec eval_ast env = function
        | Symbol(sym) -> lookup env sym
        | List(lst) -> lst |> List.map (eval env) |> List
        | Vector(arr) -> arr |> Array.map (eval env) |> Vector
        | Map(map) -> map |> Map.map (fun k v -> eval env v) |> Map
        | node -> node

    and eval env = function
        | List(_) as node ->
            let resolved = node |> eval_ast env
            match resolved with
            | List(Func({F = f})::rest) -> f rest
            | _ -> raise <| errFuncExpected ()
        | node -> node |> eval_ast env
