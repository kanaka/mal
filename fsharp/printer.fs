module Printer
    open System.Text
    open Reader
    open Types

    let pr_str data =
        let acc = StringBuilder()
        let appendStr (str : string) = acc.Append(str) |> ignore
        let rec pr_node = function
            | Nil -> appendStr "nil"
            | List(nodes) -> pr_list nodes
            | Vector(nodes) -> pr_vector nodes
            | Map(map) -> pr_map map
            | Symbol(symbol) -> appendStr symbol
            | Keyword(keyword) -> appendStr ":"; appendStr keyword
            | Number(num) -> acc.Append(num) |> ignore
            | String(str) -> pr_str str
            | Bool(true) -> appendStr "true"
            | Bool(false) -> appendStr "false"
            | Func({ Tag = tag; F = _}) -> pr_func tag

        and pr prefix node =
            appendStr prefix
            pr_node node
            " "

        and pr_str str =
            let appendChar = function
                | '\t' -> appendStr "\\t"
                | '\b' -> appendStr "\\b"
                | '\n' -> appendStr "\\n"
                | '\r' -> appendStr "\\r"
                | '\f' -> appendStr "\\f"
                | '\'' -> appendStr "\\'"
                | '"' -> appendStr "\\\""
                | '\\' -> appendStr "\\\\"
                | ch -> acc.Append(ch) |> ignore
            appendStr "\""
            str |> Seq.iter appendChar
            appendStr "\""

        and pr_func tag =
           sprintf "#<func %d>" tag |> appendStr 

        and pr_list nodes =
            appendStr "("
            nodes |> List.fold pr "" |> ignore
            appendStr ")"

        and pr_vector nodes =
            appendStr "["
            nodes |> Seq.fold pr "" |> ignore
            appendStr "]"           

        and pr_map map =
            let pr prefix key value =
                appendStr prefix
                pr_node key
                appendStr " "
                pr_node value
                " "
            appendStr "{"
            map |> Map.fold pr "" |> ignore
            appendStr "}"
        
        pr_node data
        acc.ToString()
