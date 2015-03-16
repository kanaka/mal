module Env

    open Types

    type Env = System.Collections.Generic.Dictionary<string, Node>
    type EnvChain = Env list

    let errSymbolNotFound s = EvalError(sprintf "'%s' not found" s)
    let errNoEnvironment () = EvalError("no environment")

    let makeEmpty () = Env()

    let ofList lst =
        let env = makeEmpty ()
        let accumulate (e : Env) (k, v) = e.Add(k, v); e
        List.fold accumulate env lst

    let set (env : EnvChain) key node =
        match env with
        | head::_ -> head.[key] <- node
        | _ -> raise <| errNoEnvironment ()

    let rec find (chain : EnvChain) key =
        match chain with
        | [] -> None
        | env::rest ->
            match env.TryGetValue(key) with
            | true, v -> Some(v)
            | false, _ -> find rest key

    let get chain key =
        match find chain key with
        | Some(v) -> v
        | None -> raise <| errSymbolNotFound key

    let makeRootEnv () =
        let wrap tag name func = name, Func({ Tag = tag; Name = name; F = func })
        let env =
            [ wrap 1 "+" Core.add;
              wrap 2 "-" Core.subtract;
              wrap 3 "*" Core.multiply;
              wrap 4 "/" Core.divide ]
            |> ofList
        [ env ]

