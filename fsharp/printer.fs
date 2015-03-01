module Printer
    open System.Text
    open Reader
    open Types

    let pr_str data =
        let acc = StringBuilder()
        let rec pr_node = function
            | List(nodes) 
                -> acc.Append("(") |> ignore
                   pr_list "" nodes
            | Vector(nodes)
                -> acc.Append("[") |> ignore
                   pr_vector "" nodes 0
            | Symbol(symbol) -> acc.Append(symbol) |> ignore
            | Number(num) -> acc.Append(num) |> ignore

        and pr_list prefix = function
            | head::rest 
                -> acc.Append(prefix) |> ignore
                   pr_node head
                   pr_list " " rest
            | [] -> acc.Append(")") |> ignore

        and pr_vector prefix nodes i =
            if i < nodes.Length then
                acc.Append(prefix) |> ignore
                pr_node nodes.[i]
                pr_vector " " nodes (i + 1)
            else
                acc.Append("]") |> ignore
        
        pr_node data
        acc.ToString()
