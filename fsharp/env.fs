module Env

    open Types

    let makeEmpty () = Env()

    let ofList lst =
        let env = makeEmpty ()
        let accumulate (e : Env) (k, v) = e.Add(k, v); e
        List.fold accumulate env lst

    let set (env : EnvChain) key node =
        match env with
        | head::_ -> head.[key] <- node
        | _ -> raise <| Error.noEnvironment ()

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
        | None -> raise <| Error.symbolNotFound key

    let private getNextValue =
        let counter = ref 0
        fun () -> System.Threading.Interlocked.Increment(counter)

    let makeBuiltInFunc f =
        Func(getNextValue (), f, Node.NIL, [], [])

    let makeFunc f body binds env =
        Func(getNextValue (), f, body, binds, env)

    let makeRootEnv () =
        let wrap name f = name, makeBuiltInFunc f
        let env =
            [ wrap "+" Core.add
              wrap "-" Core.subtract
              wrap "*" Core.multiply
              wrap "/" Core.divide
              wrap "list" Core.list
              wrap "list?" Core.isList
              wrap "empty?" Core.isEmpty
              wrap "count" Core.count
              wrap "=" Core.eq
              wrap "<" Core.lt
              wrap "<=" Core.le
              wrap ">=" Core.ge
              wrap ">" Core.gt
              wrap "pr-str" Core.pr_str
              wrap "str" Core.str
              wrap "prn" Core.prn
              wrap "println" Core.println
              wrap "read-string" Core.read_str
              wrap "slurp" Core.slurp
              wrap "cons" Core.cons
              wrap "concat" Core.concat ]
            |> ofList
        [ env ]

    let makeNew outer symbols nodes =
        let env = (makeEmpty ())::outer
        let rec loop symbols nodes =
            match symbols, nodes with
            | [Symbol("&"); Symbol(s)], nodes ->
                set env s (List nodes)
                env
            | Symbol("&")::_, _ -> raise <| Error.onlyOneSymbolAfterAmp ()
            | Symbol(s)::symbols, n::nodes -> 
                set env s n
                loop symbols nodes
            | [], [] -> env
            | _, [] -> raise <| Error.notEnoughValues ()
            | [], _ -> raise <| Error.tooManyValues ()
            | _, _ -> raise <| Error.expectedX "symbol"
        loop symbols nodes
