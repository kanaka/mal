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

    let makeFunc =
        let counter = ref 0
        let getNext () = System.Threading.Interlocked.Increment(counter)
        fun f -> Func({ Tag = getNext (); F = f }) 

    let makeRootEnv () =
        let wrap name f = name, makeFunc f
        let env =
            [ wrap "+" Core.add;
              wrap "-" Core.subtract;
              wrap "*" Core.multiply;
              wrap "/" Core.divide;
              wrap "list" Core.list;
              wrap "list?" Core.isList;
              wrap "empty?" Core.isEmpty;
              wrap "count" Core.count;
              wrap "=" Core.eq;
              wrap "<" Core.lt;
              wrap "<=" Core.le;
              wrap ">=" Core.ge;
              wrap ">" Core.gt ]
            |> ofList
        [ env ]

    let makeNew (env : EnvChain) = (makeEmpty ())::env
