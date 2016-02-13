module Node

    open Types

    let TRUE = Bool(true)
    let SomeTRUE = Some(TRUE)
    let FALSE = Bool(false)
    let SomeFALSE = Some(FALSE)
    let NIL = Nil
    let SomeNIL = Some(NIL)
    let ZERO = Number(0L)

    let makeVector seg = Vector(NIL, seg)
    let makeList lst = List(NIL, lst)
    let makeMap map = Map(NIL, map)

    let EmptyLIST = [] |> makeList
    let EmptyVECTOR = System.ArraySegment([| |]) |> makeVector
    let EmptyMAP = Map.empty |> makeMap

    let ofArray arr = System.ArraySegment(arr) |> makeVector

    let ofChar chr = sprintf "%c" chr |> String

    let toArray = function
        | List(_, lst) -> Array.ofList lst
        | Vector(_, seg) -> Array.sub seg.Array seg.Offset seg.Count
        | node -> [| node |]

    let length = function
        | List(_, lst) -> List.length lst
        | Vector(_, seg) -> seg.Count
        | Map(_, m) -> m.Count
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
        | List(_, lst) -> accumList (Array.zeroCreate num) 0 lst
        | Vector(_, seg) when seg.Count = num -> Some(toArray node)
        | _ -> None

    let inline (|Cons|_|) node =
        match node with
        | List(_, h::t) -> Some(Cons(h, makeList t))
        | Vector(_, seg) when seg.Count > 0 ->
            let h = seg.Array.[seg.Offset]
            let t = System.ArraySegment(seg.Array, seg.Offset + 1, seg.Count - 1)
                    |> makeVector
            Some(Cons(h, t))
        | _ -> None

    let inline (|Empty|_|) node =
        match node with
        | List(_, []) -> Some(Empty)
        | Vector(_, seg) when seg.Count = 0 -> Some(Empty)
        | _ -> None

    let inline (|Pair|_|) node =
        match node with
        | List(_, a::b::t) -> Some(a, b, makeList t)
        | List(_, []) -> None
        | List(_, _) -> raise <| Error.expectedEvenNodeCount ()
        | Vector(_, seg) ->
            match seg.Count with
            | 0 -> None
            | 1 -> raise <| Error.expectedEvenNodeCount ()
            | _ ->
                let a = seg.Array.[seg.Offset]
                let b = seg.Array.[seg.Offset + 1]
                let t = System.ArraySegment(seg.Array, seg.Offset + 2, seg.Count - 2)
                        |> makeVector
                Some(a, b, t)
        | _ -> None

    let inline (|Seq|_|) node =
        match node with
        | List(_, lst) -> Some(Seq.ofList lst)
        | Vector(_, seg) -> Some(seg :> Node seq)
        | _ -> None
