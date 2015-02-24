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
            | Symbol(symbol) -> acc.Append(symbol) |> ignore
            | Number(num) -> acc.Append(num) |> ignore

        and pr_list prefix = function
            | head::rest 
                -> acc.Append(prefix) |> ignore
                   pr_node head
                   pr_list " " rest
            | [] -> acc.Append(")") |> ignore
        
        pr_node data
        acc.ToString()
