module Node

    open Types

    let TRUE = Bool(true)
    let SomeTRUE = Some(TRUE)
    let FALSE = Bool(false)
    let SomeFALSE = Some(FALSE)
    let NIL = Nil
    let SomeNIL = Some(NIL)
    let ZERO = Number(0L)

    let ofArray arr = System.ArraySegment(arr) |> Vector

    let toArray = function
        | List(lst) -> Array.ofList lst
        | Vector(seg) -> Array.sub seg.Array seg.Offset seg.Count
        | node -> [| node |]

    let length = function
        | List(lst) -> List.length lst
        | Vector(seg) -> seg.Count
        | Map(m) -> m.Count
        | _ -> 1

    (* Active Patterns to help with pattern matching nodes *)
    let inline (|Elements|_|) num node =
        let rec accumList acc idx lst =
            let len = Array.length acc
            match lst with
            | [] when idx = len -> Some(Elements acc)
            | h::t when idx < len ->
                acc.[idx] <- h
                accumList acc (idx + 1) t
            | _ -> None
        match node with
        | List(lst) -> accumList (Array.zeroCreate num) 0 lst
        | Vector(seg) when seg.Count = num -> Some(toArray node)
        | _ -> None

    let inline (|Cons|_|) node =
        match node with
        | List(h::t) -> Some(Cons(h, List(t)))
        | Vector(seg) when seg.Count > 0 ->
            let h = seg.Array.[seg.Offset]
            let t = System.ArraySegment(seg.Array, seg.Offset + 1, seg.Count - 1)
                    |> Vector
            Some(Cons(h, t))
        | _ -> None

    let inline (|Empty|_|) node =
        match node with
        | List([]) -> Some(Empty)
        | Vector(seg) when seg.Count = 0 -> Some(Empty)
        | _ -> None

    let inline (|Pair|_|) node =
        match node with
        | List(a::b::t) -> Some(a, b, List(t))
        | List([]) -> None
        | List(_) -> raise <| Error.expectedEvenNodeCount ()
        | Vector(seg) ->
            match seg.Count with
            | 0 -> None
            | 1 -> raise <| Error.expectedEvenNodeCount ()
            | _ ->
                let a = seg.Array.[seg.Offset]
                let b = seg.Array.[seg.Offset + 1]
                let t = System.ArraySegment(seg.Array, seg.Offset + 2, seg.Count - 2)
                        |> Vector
                Some(a, b, t)
        | _ -> None
