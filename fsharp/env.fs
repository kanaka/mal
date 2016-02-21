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
        BuiltInFunc(Node.NIL, getNextValue (), f)

    let makeFunc f body binds env =
        Func(Node.NIL, getNextValue (), f, body, binds, env)

    let makeMacro f body binds env =
        Macro(Node.NIL, getNextValue (), f, body, binds, env)

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
              wrap "time-ms" Core.time_ms
              wrap "pr-str" Core.pr_str
              wrap "str" Core.str
              wrap "prn" Core.prn
              wrap "println" Core.println
              wrap "read-string" Core.read_str
              wrap "slurp" Core.slurp
              wrap "cons" Core.cons
              wrap "concat" Core.concat
              wrap "nth" Core.nth
              wrap "first" Core.first
              wrap "rest" Core.rest
              wrap "throw" Core.throw
              wrap "map" Core.map
              wrap "apply" Core.apply
              wrap "nil?" (Core.isConst Node.NIL)
              wrap "true?" (Core.isConst Node.TRUE)
              wrap "false?" (Core.isConst Node.FALSE)
              wrap "symbol?" Core.isSymbol
              wrap "symbol" Core.symbol
              wrap "keyword?" Core.isKeyword
              wrap "keyword" Core.keyword
              wrap "sequential?" Core.isSequential
              wrap "vector?" Core.isVector
              wrap "vector" Core.vector
              wrap "map?" Core.isMap
              wrap "hash-map" Core.hashMap
              wrap "assoc" Core.assoc
              wrap "dissoc" Core.dissoc
              wrap "get" Core.get
              wrap "contains?" Core.contains
              wrap "keys" Core.keys
              wrap "vals" Core.vals
              wrap "atom" (Core.atom getNextValue)
              wrap "atom?" Core.isAtom
              wrap "deref" Core.deref
              wrap "reset!" Core.reset
              wrap "swap!" Core.swap
              wrap "conj" Core.conj
              wrap "meta" Core.meta
              wrap "with-meta" Core.withMeta ]
            |> ofList
        [ env ]

    let makeNew outer symbols nodes =
        let env = (makeEmpty ())::outer
        let rec loop symbols nodes =
            match symbols, nodes with
            | [Symbol("&"); Symbol(s)], nodes ->
                set env s (Node.makeList nodes)
                env
            | Symbol("&")::_, _ -> raise <| Error.onlyOneSymbolAfterAmp ()
            | Symbol(s)::symbols, n::nodes -> 
                set env s n
                loop symbols nodes
            | [], [] -> env
            | _, [] -> raise <| Error.notEnoughValues ()
            | [], _ -> raise <| Error.tooManyValues ()
            | _, _ -> raise <| Error.errExpectedX "symbol"
        loop symbols nodes

    (* Active Patterns to help with pattern matching nodes *)
    let inline (|IsMacro|_|) env = function
        | List(_, Symbol(sym)::rest) ->
            match find env sym with
            | Some(Macro(_, _, _, _, _, _) as m) -> Some(IsMacro m, rest)
            | _ -> None
        | _ -> None
