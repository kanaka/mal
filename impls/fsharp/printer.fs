module Printer
    open System.Text
    open Types

    type Profile = { Pretty : bool; Separator : string }
    let pr_str_profile = { Pretty = true; Separator = " " }
    let str_profile = { Pretty = false; Separator = "" }
    let prn_profile = { Pretty = true; Separator = " " }
    let println_profile = { Pretty = false; Separator = " " }
    
    let print profile nodes =
        let acc = StringBuilder()
        let appendStr (str : string) = acc.Append(str) |> ignore
        let rec pr_node = function
            | Nil -> appendStr "nil"
            | List(_, nodes) -> pr_list nodes
            | Vector(_, nodes) -> pr_vector nodes
            | Map(_, map) -> pr_map map
            | Symbol(symbol) -> appendStr symbol
            | Keyword(keyword) -> appendStr ":"; appendStr keyword
            | Number(num) -> acc.Append(num) |> ignore
            | String(str) when profile.Pretty -> pr_str_pretty str
            | String(str) -> appendStr str
            | Bool(true) -> appendStr "true"
            | Bool(false) -> appendStr "false"
            | BuiltInFunc(_, tag, _) | Func(_, tag, _, _, _, _) ->
                pr_func "func" tag
            | Macro(_, tag, _, _, _, _) -> pr_func "macro" tag
            | Atom(tag, r) -> pr_atom tag !r

        and pr separator prefix node =
            appendStr prefix
            pr_node node
            separator

        and std_pr = pr " "

        and pr_str_pretty str =
            let appendChar = function
                | '\t' -> appendStr "\\t"
                | '\b' -> appendStr "\\b"
                | '\n' -> appendStr "\\n"
                | '\r' -> appendStr "\\r"
                | '\f' -> appendStr "\\f"
                | '"' -> appendStr "\\\""
                | '\\' -> appendStr "\\\\"
                | ch -> acc.Append(ch) |> ignore
            appendStr "\""
            str |> Seq.iter appendChar
            appendStr "\""

        and pr_func ftype tag =
            sprintf "#<%s %d>" ftype tag |> appendStr

        and pr_atom tag node =
            appendStr "(atom "
            pr_node node
            appendStr ")"

        and pr_list nodes =
            appendStr "("
            nodes |> List.fold std_pr  "" |> ignore
            appendStr ")"

        and pr_vector nodes =
            appendStr "["
            nodes |> Seq.fold std_pr "" |> ignore
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
        
        nodes |> Seq.fold (pr profile.Separator) "" |> ignore
        acc.ToString()

    let pr_str : seq<Node> -> string = print pr_str_profile
    let str : seq<Node> -> string = print str_profile
    let prn : seq<Node> -> string = print prn_profile
    let println : seq<Node> -> string = print println_profile
