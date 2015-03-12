module Types

    exception ReaderError of string
    exception EvalError of string

    type 
        Node =
        | Nil
        | List of Node list
        | Vector of Node array
        | Map of Collections.Map<Node, Node>
        | Symbol of string
        | Keyword of string
        | Number of int64
        | String of string
        | Bool of bool
        | Func of F
    and
        // Compare only on Tag and Name since functions are not IComparable
        [<CustomEquality; CustomComparison>]
        F = { Tag : int; Name : string; F : Node list -> Node }

            override x.Equals(yobj) =
                match yobj with
                | :? F as y -> x.Tag = y.Tag && x.Name = y.Name
                | _ -> false

            override x.GetHashCode() =
                ((hash x.Tag) * 397) ^^^ (hash x.Name)

            interface System.IComparable with
                member x.CompareTo yobj =
                    match yobj with
                    | :? F as y -> 
                        let a = compare x.Tag y.Tag
                        if a <> 0 then a
                        else compare x.Name y.Name
                    | _ -> invalidArg "yobj" "Cannot compare values of different types."
