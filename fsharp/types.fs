module Types

    [<CustomEquality; CustomComparison>]
    type Node =
        | Nil
        | List of Node list
        | Vector of Node System.ArraySegment
        | Map of Collections.Map<Node, Node>
        | Symbol of string
        | Keyword of string
        | Number of int64
        | String of string
        | Bool of bool
        | BuiltInFunc of int * (Node list -> Node)
        | Func of int * (Node list -> Node) * Node * Node list * EnvChain
        | Macro of int * (Node list -> Node) * Node * Node list * EnvChain

        static member private hashSeq (s : seq<Node>) =
            let iter st node = (st * 397) ^^^ node.GetHashCode()
            s |> Seq.fold iter 0

        static member private allEqual (x : seq<Node>) (y : seq<Node>) =
            use ex = x.GetEnumerator()
            use ey = y.GetEnumerator()
            let rec loop () =
                match ex.MoveNext(), ey.MoveNext() with
                | false, false -> true
                | false, true
                | true, false -> false
                | true, true ->
                    if ex.Current = ey.Current then
                        loop ()
                    else
                        false
            loop ()

        static member private allCompare (x : seq<Node>) (y : seq<Node>) =
            use ex = x.GetEnumerator()
            use ey = y.GetEnumerator()
            let rec loop () =
                match ex.MoveNext(), ey.MoveNext() with
                | false, false -> 0
                | false, true -> -1
                | true, false -> 1
                | true, true ->
                    let cmp = compare ex.Current ey.Current
                    if cmp = 0 then loop () else cmp
            loop ()

        static member private rank x =
            match x with
            | Nil -> 0
            | List(_) -> 1
            | Vector(_) -> 2
            | Map(_) -> 3
            | Symbol(_) -> 4
            | Keyword(_) -> 5
            | Number(_) -> 6
            | String(_) -> 7
            | Bool(_) -> 8
            | BuiltInFunc(_, _)
            | Func(_, _, _, _, _)
            | Macro(_, _, _, _, _) -> 9

        static member private equals x y =
            match x, y with
            | Nil, Nil -> true
            | List(a), List(b) -> a = b
            | List(a), Vector(b) -> Node.allEqual a b
            | Vector(a), List(b) -> Node.allEqual a b
            | Vector(a), Vector(b) -> Node.allEqual a b
            | Map(a), Map(b) -> a = b
            | Symbol(a), Symbol(b) -> a = b
            | Keyword(a), Keyword(b) -> a = b
            | Number(a), Number(b) -> a = b
            | String(a), String(b) -> a = b
            | Bool(a), Bool(b) -> a = b
            | (BuiltInFunc(a, _) | Func(a, _, _, _, _) | Macro(a, _, _, _, _)),
              (BuiltInFunc(b, _) | Func(b, _, _, _, _) | Macro(b, _, _, _, _)) ->
                a = b
            | _, _ -> false

        static member private compare x y =
            match x, y with
            | Nil, Nil -> 0
            | List(a), List(b) -> compare a b
            | List(a), Vector(b) -> Node.allCompare a b
            | Vector(a), List(b) -> Node.allCompare a b
            | Vector(a), Vector(b) -> Node.allCompare a b
            | Map(a), Map(b) -> compare a b
            | Symbol(a), Symbol(b) -> compare a b
            | Keyword(a), Keyword(b) -> compare a b
            | Number(a), Number(b) -> compare a b
            | String(a), String(b) -> compare a b
            | Bool(a), Bool(b) -> compare a b
            | (BuiltInFunc(a, _) | Func(a, _, _, _, _) | Macro(a, _, _, _, _)),
              (BuiltInFunc(b, _) | Func(b, _, _, _, _) | Macro(b, _, _, _, _)) ->
                compare a b
            | a, b -> compare (Node.rank a) (Node.rank b)

        override x.Equals yobj =
            match yobj with
            | :? Node as y -> Node.equals x y
            | _ -> false

        override x.GetHashCode() =
            match x with
            | Nil -> 0
            | List(lst) -> hash lst
            | Vector(vec) -> Node.hashSeq vec
            | Map(map) -> hash map
            | Symbol(sym) -> hash sym
            | Keyword(key) -> hash key
            | Number(num) -> hash num
            | String(str) -> hash str
            | Bool(b) -> hash b
            | BuiltInFunc(tag, _) | Func(tag, _, _, _, _) | Macro(tag, _, _, _, _) ->
                hash tag

        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? Node as y -> Node.compare x y
                | _ -> invalidArg "yobj" "Cannot compare values of different types."


    and Env = System.Collections.Generic.Dictionary<string, Node>
    and EnvChain = Env list
