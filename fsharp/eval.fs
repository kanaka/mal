module Eval

    open Types

    type Env = Map<string, Node>

    let int64op nodes op st =
        let f s node =
            match node with
            | Number(n) -> op s n
            | _ -> raise (EvalError("argument mismatch"))
        nodes
        |> List.fold f st
        |> Number

    let add nodes = int64op nodes (fun a b -> a + b) 0L
    let sub nodes = 
        match nodes with
        | [] -> raise (EvalError("wrong number of args"))
        | Number(first)::[] -> Number(-first)
        | Number(first)::rest -> int64op rest (fun a b -> a - b) first
        | _ -> raise (EvalError("argument mismatch"))
    let mul nodes = int64op nodes (fun a b -> a * b) 1L
    let div nodes = 
        match nodes with
        | [] -> raise (EvalError("wrong number of args"))
        | Number(first)::[] -> Number(1L / first)
        | Number(first)::rest -> int64op rest (fun a b -> a / b) first
        | _ -> raise (EvalError("argument mismatch"))

    let makeEnv () =
        [ "+", Func({ Tag = 1; Name = "+"; F = add });
          "-", Func({ Tag = 2; Name = "-"; F = sub });
          "*", Func({ Tag = 3; Name = "*"; F = mul });
          "/", Func({ Tag = 4; Name = "/"; F = div }) ]
        |> Map.ofList

    let lookup (env : Env) sym =
        match env.TryFind sym with
        | Some(f) -> f
        | None -> raise (EvalError(sprintf "'%s' not found" sym))

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
            | _ -> raise (EvalError(sprintf "Expected function"))
        | node -> node |> eval_ast env
