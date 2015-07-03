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

    let list nodes = List(nodes)
    let isList = function
        | [List(_)] -> Node.TRUE
        | [_] -> Node.FALSE
        | _ -> raise <| Error.wrongArity ()

    let isEmpty = function
        | [List([])] -> Node.TRUE
        | [Vector(seg)] when seg.Count <= 0 -> Node.TRUE
        | _ -> Node.FALSE

    let count = function
        | [List(lst)] -> lst |> List.length |> int64 |> Number
        | [Vector(seg)] -> seg.Count |> int64 |> Number
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
            | nodes -> List(Symbol("do")::nodes)
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let slurp = function
        | [String(s)] -> System.IO.File.ReadAllText s |> String
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let cons = function
        | [node; List(lst)] -> List(node::lst)
        | [node; Vector(seg)] -> List(node::(List.ofSeq seg))
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let concat nodes =
        let cons st node = node::st
        let accumNode acc = function
            | List(lst) -> lst |> List.fold cons acc
            | Vector(seg) -> seg |> Seq.fold cons acc
            | _ -> raise <| Error.argMismatch ()

        nodes
        |> List.fold accumNode []
        |> List.rev
        |> List

    let nth = function
        | [List(lst); Number(n)] ->
            let rec nth_list n = function
                | [] -> raise <| Error.indexOutOfBounds ()
                | h::_ when n = 0L -> h
                | _::t -> nth_list (n - 1L) t
            nth_list n lst
        | [Vector(seg); Number(n)] ->
            if n < 0L || n >= int64(seg.Count) then
                raise <| Error.indexOutOfBounds ()
            else
                seg.Array.[int(n)]
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let first = function
        | [List([])] -> Node.NIL
        | [List(h::_)] -> h
        | [Vector(seg)] when seg.Count > 0 -> seg.Array.[0]
        | [Vector(_)] -> Node.NIL
        | [Nil] -> Node.NIL
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let rest = function
        | [List([]) as lst] -> lst
        | [List(_::t)] -> List(t)
        | [Vector(seg)] when seg.Count < 2 -> Node.EmptyLIST
        | [Vector(seg)] -> seg |> Seq.skip 1 |> List.ofSeq |> List
        | [_] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let throw = function
        | [node] -> raise <| Error.MalError(node)
        | _ -> raise <| Error.wrongArity ()

    let map = function
        | [BuiltInFunc(_, f); Node.Seq seq]
        | [Func(_, f, _, _, _); Node.Seq seq] ->
            seq |> Seq.map (fun node -> f [node]) |> List.ofSeq |> List
        | [_; _] -> raise <| Error.argMismatch ()
        | _ -> raise <| Error.wrongArity ()

    let apply = function
        | BuiltInFunc(_, f)::rest
        | Func(_, f, _, _, _)::rest ->
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
    let isVector = isPattern (function Vector(_) -> true | _ -> false)
    let isMap = isPattern (function Map(_) -> true | _ -> false)

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
        | Map(map)::rest -> f rest map
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
        mapOp0 (fun map -> map |> Map.toSeq |> Seq.map f |> List.ofSeq |> List)

    let hashMap lst = lst |> getPairs |> Map.ofSeq |> Map
    let assoc = mapOpN (fun rest map ->
                            rest
                            |> getPairs
                            |> Seq.fold (fun map (k, v) -> Map.add k v map) map
                            |> Map)
    let dissoc = mapOpN (fun keys map ->
                            keys
                            |> List.fold (fun map k -> Map.remove k map) map
                            |> Map)
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
