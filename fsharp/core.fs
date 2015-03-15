module Core
    
    open Types   

    let errArity () = EvalError("arity: wrong number of arguments")
    let errArgMismatch () = EvalError("argument mismatch")

    let inline toNumber node =
        match node with
        | Number(n) -> n
        | _ -> raise <| errArgMismatch ()

    let inline makeNumFolder op =
        (fun state node -> op state (node |> toNumber))
     
    let add nodes =
        let addNode = makeNumFolder (+)
        nodes |> Seq.fold addNode 0L |> Number

    let subtract nodes =
        let subtractNode = makeNumFolder (-)
        match nodes with
        | [] -> raise <| errArity ()
        | Number(first)::[] -> Number(-first)
        | Number(first)::rest -> rest |> Seq.fold subtractNode first |> Number
        | _ -> raise <| errArgMismatch ()

    let multiply nodes =
        let multiplyNode = makeNumFolder ( * )
        nodes |> Seq.fold multiplyNode 1L |> Number

    let divide nodes =
        let divideNode = makeNumFolder (/)
        match nodes with
        | [] -> raise <| errArity ()
        | Number(first)::[] -> 1L / first |> Number
        | Number(first)::rest -> rest |> Seq.fold divideNode first |> Number
        | _ -> raise <| errArgMismatch ()
