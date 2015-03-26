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
        // Compare only on Tag since functions are not IComparable
        [<CustomEquality; CustomComparison>]
        F = { Tag : int; F : Node list -> Node }

            override x.Equals(yobj) =
                match yobj with
                | :? F as y -> x.Tag = y.Tag
                | _ -> false

            override x.GetHashCode() = hash x.Tag

            interface System.IComparable with
                member x.CompareTo yobj =
                    match yobj with
                    | :? F as y -> compare x.Tag y.Tag
                    | _ -> invalidArg "yobj" "Cannot compare values of different types."

    let TRUE = Bool(true)
    let SomeTRUE = Some(TRUE)
    let FALSE = Bool(false)
    let SomeFALSE = Some(FALSE)
    let NIL = Nil
    let SomeNIL = Some(NIL)
    let ZERO = Number(0L)
