module Core
    
    open Types

    let errArity () = EvalError("arity: wrong number of arguments")
    let errArgMismatch () = EvalError("argument mismatch")

    let inline toBool b = if b then TRUE else FALSE

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
        | [List(_)] -> TRUE
        | [_] -> FALSE
        | _ -> raise <| errArity ()

    let isEmpty = function
        | [List([])]
        | [Vector([||])] -> TRUE
        | _ -> FALSE

    let count = function
        | [List(lst)] -> lst |> List.length |> int64 |> Number
        | [Vector(vec)] -> vec |> Array.length |> int64 |> Number
        | [Nil] -> ZERO
        | [_] -> raise <| errArgMismatch ()
        | _ -> raise <| errArity ()

    let pr_str nodes = nodes |> Printer.pr_str |> String
    let str nodes = nodes |> Printer.str |> String
    let prn nodes = nodes |> Printer.prn |> printfn "%s"; Nil
    let println nodes = nodes |> Printer.println |> printfn "%s"; Nil
