module Core
    
    open Types

    let inline toBool b = if b then Node.TRUE else Node.FALSE

    let inline twoNumberOp (f : int64 -> int64 -> Node) = function
        | [Number(a); Number(b)] -> f a b
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()
    
    let inline twoNodeOp (f : Node -> Node -> Node) = function
        | [a; b] -> f a b
        | _ -> raise <| Error.wrongArity ()
 
    let add = twoNumberOp (fun a b -> a + b |> Number)
    let subtract = twoNumberOp (fun a b -> a - b |> Number)
    let multiply = twoNumberOp (fun a b -> a * b |> Number)
    let divide = twoNumberOp (fun a b -> a / b |> Number)
    let lt = twoNodeOp (fun a b -> a < b |> toBool)
    let le = twoNodeOp (fun a b -> a <= b |> toBool)
    let ge = twoNodeOp (fun a b -> a >= b |> toBool)
    let gt = twoNodeOp (fun a b -> a > b |> toBool)
    let eq = twoNodeOp (fun a b -> a = b |> toBool)

    let list = Node.makeList
    let isList = function
        | [List(_, _)] -> Node.TRUE
        | [_] -> Node.FALSE
        | _ -> raise <| Error.wrongArity ()

    let isEmpty = function
        | [List(_, [])] -> Node.TRUE
        | [Vector(_, seg)] when seg.Count <= 0 -> Node.TRUE
        | _ -> Node.FALSE

    let count = function
        | [List(_, lst)] -> lst |> List.length |> int64 |> Number
        | [Vector(_, seg)] -> seg.Count |> int64 |> Number
        | [Nil] -> Node.ZERO
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let pr_str nodes = nodes |> Printer.pr_str |> String
    let str nodes = nodes |> Printer.str |> String
    let prn nodes = nodes |> Printer.prn |> printfn "%s"; Nil
    let println nodes = nodes |> Printer.println |> printfn "%s"; Nil

    let read_str = function
        | [String(s)] ->
            match Reader.read_str s with
            | [node] -> node
            | nodes -> Symbol("do")::nodes |> Node.makeList
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let slurp = function
        | [String(s)] -> System.IO.File.ReadAllText s |> String
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let cons = function
        | [node; List(_, lst)] -> node::lst |> Node.makeList
        | [node; Vector(_, seg)] -> node::(List.ofSeq seg) |> Node.makeList
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let concat nodes =
        let cons st node = node::st
        let accumNode acc = function
            | List(_, lst) -> lst |> List.fold cons acc
            | Vector(_, seg) -> seg |> Seq.fold cons acc
            | _ -> raise <| Error.argMismatch ()

        nodes
        |> List.fold accumNode []
        |> List.rev
        |> Node.makeList

    let nth = function
        | [List(_, lst); Number(n)] ->
            let rec nth_list n = function
                | [] -> raise <| Error.indexOutOfBounds ()
                | h::_ when n = 0L -> h
                | _::t -> nth_list (n - 1L) t
            nth_list n lst
        | [Vector(_, seg); Number(n)] ->
            if n < 0L || n >= int64(seg.Count) then
                raise <| Error.indexOutOfBounds ()
            else
                seg.Array.[int(n)]
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let first = function
        | [List(_, [])] -> Node.NIL
        | [List(_, h::_)] -> h
        | [Vector(_, seg)] when seg.Count > 0 -> seg.Array.[0]
        | [Vector(_, _)] -> Node.NIL
        | [Nil] -> Node.NIL
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let rest = function
        | [List(_, [])] -> Node.EmptyLIST
        | [List(_, _::t)] -> t |> Node.makeList
        | [Vector(_, seg)] when seg.Count < 2 -> Node.EmptyLIST
        | [Vector(_, seg)] -> seg |> Seq.skip 1 |> List.ofSeq |> Node.makeList
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let throw = function
        | [node] -> raise <| Error.MalError(node)
        | _ -> raise <| Error.wrongArity ()

    let map = function
        | [BuiltInFunc(_, _, f); Node.Seq seq]
        | [Func(_, _, f, _, _, _); Node.Seq seq] ->
            seq |> Seq.map (fun node -> f [node]) |> List.ofSeq |> Node.makeList
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let apply = function
        | BuiltInFunc(_, _, f)::rest
        | Func(_, _, f, _, _, _)::rest ->
            let rec getArgsAndCall acc = function
                | [] -> raise <| Error.wrongArity ()
                | [Node.Seq seq] ->
                    seq |> Seq.fold (fun acc node -> node::acc) acc |> List.rev |> f
                | [_] -> raise <| Error.argMismatch ()
                | h::rest -> getArgsAndCall (h::acc) rest
            getArgsAndCall [] rest
        | _::_ -> raise <| Error.argMismatch ()
        | [] -> raise <| Error.wrongArity ()

    let isConst cmp = function
        | [node] -> if node = cmp then Node.TRUE else Node.FALSE
        | _ -> raise <| Error.wrongArity ()

    let isPattern f = function
        | [node] -> if f node then Node.TRUE else Node.FALSE
        | _ -> raise <| Error.wrongArity ()

    let isSymbol = isPattern (function Symbol(_) -> true | _ -> false)
    let isKeyword = isPattern (function Keyword(_) -> true | _ -> false)
    let isSequential = isPattern (function Node.Seq(_) -> true | _ -> false)
    let isVector = isPattern (function Vector(_, _) -> true | _ -> false)
    let isMap = isPattern (function Map(_, _) -> true | _ -> false)
    let isAtom = isPattern (function Atom(_, _) -> true | _ -> false)

    let fromString f = function
        | [String(str)] -> f str
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let symbol = fromString (fun s -> Symbol(s))
    let keyword = fromString (fun s -> Keyword(s))
    let vector lst =  lst |> Array.ofList |> Node.ofArray

    let rec getPairs lst =
        seq {
            match lst with
            | first::second::t ->
                yield first, second
                yield! getPairs t
            | [_] -> raise <| Error.expectedEvenNodeCount ()
            | [] -> ()
        }

    let mapOpN f = function
        | Map(_, map)::rest -> f rest map
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let mapOp1 f =
        mapOpN (fun rest map ->
                    match rest with
                    | [v] -> f v map
                    | _ -> raise <| Error.wrongArity ())

    let mapOp0 f =
        mapOpN (fun rest map ->
                    match rest with
                    | [] -> f map
                    | _ -> raise <| Error.wrongArity ())

    let mapKV f =
        mapOp0 (fun map -> map |> Map.toSeq |> Seq.map f |> List.ofSeq |> Node.makeList)

    let hashMap lst = lst |> getPairs |> Map.ofSeq |> Node.makeMap
    let assoc = mapOpN (fun rest map ->
                            rest
                            |> getPairs
                            |> Seq.fold (fun map (k, v) -> Map.add k v map) map
                            |> Node.makeMap)
    let dissoc = mapOpN (fun keys map ->
                            keys
                            |> List.fold (fun map k -> Map.remove k map) map
                            |> Node.makeMap)
    let get = function
        | [Nil; _] -> Node.NIL
        | _ as rest ->
            rest |> mapOp1 (fun key map ->
                                match Map.tryFind key map with
                                | Some(node) -> node
                                | None -> Node.NIL)
    let containsKey key map = if Map.containsKey key map then Node.TRUE else Node.FALSE
    let contains = mapOp1 containsKey
    let keys = mapKV (fun (k, v) -> k)
    let vals = mapKV (fun (k, v) -> v)

    let atom nextValue = function
        | [node] -> Atom((nextValue ()), ref node)
        | _ -> raise <| Error.wrongArity ()

    let deref = function
        | [Atom(_, r)] -> !r
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let reset = function
        | [Atom(_, r); node] ->
            r := node
            !r
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let swap = function
        | Atom(_, r)
            ::(BuiltInFunc(_, _, f) | Func(_, _, f, _, _, _))
            ::rest ->
                r := f (!r::rest)
                !r
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let conj = function
        | List(_, lst)::rest ->
            rest 
            |> List.fold (fun lst node -> node::lst) lst 
            |> Node.makeList
        | Vector(_, seg)::rest ->
            (* Might be nice to implement a persistent vector here someday. *)
            let cnt = List.length rest
            if cnt > 0 then
                let target : Node array = seg.Count + cnt |> Array.zeroCreate
                System.Array.Copy(seg.Array :> System.Array, seg.Offset, 
                    target :> System.Array, 0, seg.Count)
                let rec copyElem i = function
                    | h::t -> 
                        Array.set target i h
                        copyElem (i + 1) t
                    | [] -> ()
                copyElem (seg.Count) rest
                target |> Node.ofArray
            else
                seg |> Node.makeVector
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let withMeta = function
        | [List(_, lst); m] -> List(m, lst)
        | [Vector(_, seg); m] -> Vector(m, seg)
        | [Map(_, map); m] -> Map(m, map)
        | [BuiltInFunc(_, tag, f); m] -> BuiltInFunc(m, tag, f)
        | [Func(_, tag, f, a, b, c); m] -> Func(m, tag, f, a, b, c)
        | [Macro(_, tag, f, a, b, c); m] -> Macro(m, tag, f, a, b, c)
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let meta = function
        | [List(m, _)]
        | [Vector(m, _)]
        | [Map(m, _)]
        | [BuiltInFunc(m, _, _)]
        | [Func(m, _, _, _, _, _)]
        | [Macro(m, _, _, _, _, _)] -> m
        | [_] -> Node.NIL
        | _ -> raise <| Error.wrongArity ()
