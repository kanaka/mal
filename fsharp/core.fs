module Core
    
    open Types

    let errArity () = EvalError("arity: wrong number of arguments")
    let errArgMismatch () = EvalError("argument mismatch")

    let inline toBool b = if b then Node.TRUE else Node.FALSE

    let inline twoNumberOp (f : int64 -> int64 -> Node) = function
        | [Number(a); Number(b)] -> f a b
        | [_; _] -> raise <| errArgMismatch ()
        | _ -> raise <| errArity ()
    
    let inline twoNodeOp (f : Node -> Node -> Node) = function
        | [a; b] -> f a b
        | _ -> raise <| errArity ()
 
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
        | _ -> raise <| errArity ()

    let isEmpty = function
        | [List([])] -> Node.TRUE
        | [Vector(seg)] when seg.Count <= 0 -> Node.TRUE
        | _ -> Node.FALSE

    let count = function
        | [List(lst)] -> lst |> List.length |> int64 |> Number
        | [Vector(seg)] -> seg.Count |> int64 |> Number
        | [Nil] -> Node.ZERO
        | [_] -> raise <| errArgMismatch ()
        | _ -> raise <| errArity ()

    let pr_str nodes = nodes |> Printer.pr_str |> String
    let str nodes = nodes |> Printer.str |> String
    let prn nodes = nodes |> Printer.prn |> printfn "%s"; Nil
    let println nodes = nodes |> Printer.println |> printfn "%s"; Nil

    let read_str = function
        | [String(s)] ->
            match Reader.read_str s with
            | [node] -> node
            | nodes -> List(Symbol("do")::nodes)
        | [_] -> raise <| errArgMismatch ()
        | _ -> raise <| errArity ()

    let slurp = function
        | [String(s)] -> System.IO.File.ReadAllText s |> String
        | [_] -> raise <| errArgMismatch ()
        | _ -> raise <| errArity ()

    let cons = function
        | [node; List(lst)] -> List(node::lst)
        | [node; Vector(seg)] -> List(node::(List.ofSeq seg))
        | [_; _] -> raise <| errArgMismatch ()
        | _ -> raise <| errArity ()

    let concat nodes =
        let cons st node = node::st
        let accumNode acc = function
            | List(lst) -> lst |> List.fold cons acc
            | Vector(seg) -> seg |> Seq.fold cons acc
            | _ -> raise <| errArgMismatch ()

        nodes
        |> List.fold accumNode []
        |> List.rev
        |> List
