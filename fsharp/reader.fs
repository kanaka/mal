module Reader
    open System
    open Tokenizer
    open Types
    open Node

    type MutableList = System.Collections.Generic.List<Node>
    let inline addToMutableList (lst:MutableList) item = lst.Add(item); lst

    let quote = Symbol("quote")
    let quasiquote = Symbol("quasiquote")
    let unquote = Symbol("unquote")
    let spliceUnquote = Symbol("splice-unquote")
    let deref = Symbol("deref")
    let withMeta = Symbol("with-meta")

    let rec readForm = function
        | OpenParen::rest -> readList [] rest
        | OpenBracket::rest -> readVector (MutableList()) rest
        | OpenBrace::rest -> readMap [] rest
        | SingleQuote::rest -> wrapForm quote rest
        | Backtick::rest -> wrapForm quasiquote rest
        | Tilde::rest -> wrapForm unquote rest
        | SpliceUnquote::rest -> wrapForm spliceUnquote rest
        | At::rest -> wrapForm deref rest
        | Caret::rest -> readMeta rest
        | tokens -> readAtom tokens

    and wrapForm node tokens = 
        match readForm tokens with
        | Some(form), rest -> Some(makeList [node; form]), rest
        | None, _ -> raise <| Error.expectedXButEOF "form"

    and readList acc = function
        | CloseParen::rest -> Some(acc |> List.rev |> makeList), rest
        | [] -> raise <| Error.expectedXButEOF "')'"
        | tokens -> 
            match readForm tokens with
            | Some(form), rest -> readList (form::acc) rest
            | None, _ -> raise <| Error.expectedXButEOF "')'"

    and readVector acc = function
        | CloseBracket::rest -> Some(acc.ToArray() |> Node.ofArray), rest
        | [] -> raise <| Error.expectedXButEOF "']'"
        | tokens -> 
            match readForm tokens with
            | Some(form), rest -> readVector (addToMutableList acc form) rest
            | None, _ -> raise <| Error.expectedXButEOF "']'"

    and readMap acc = function
        | CloseBrace::rest -> Some(acc |> List.rev |> Map.ofList |> makeMap), rest
        | [] -> raise <| Error.expectedXButEOF "'}'"
        | tokens -> 
            match readForm tokens with
            | Some(key), rest ->
                match readForm rest with
                | Some(v), rest -> readMap ((key, v)::acc) rest
                | None, _ -> raise <| Error.expectedXButEOF "'}'"
            | None, _ -> raise <| Error.expectedXButEOF "'}'"

    and readMeta = function
        | OpenBrace::rest ->
            let meta, rest = readMap [] rest
            match readForm rest with
            | Some(form), rest -> Some([withMeta; form; meta.Value] |> makeList), rest
            | None, _ -> raise <| Error.expectedXButEOF "form"
        | _ -> raise <| Error.expectedXButEOF "map"

    and readAtom = function
        | Token("nil")::rest -> Node.SomeNIL, rest
        | Token("true")::rest -> Node.SomeTRUE, rest
        | Token("false")::rest -> Node.SomeFALSE, rest
        | Tokenizer.String(str)::rest -> Some(String(str)), rest
        | Tokenizer.Keyword(kw)::rest -> Some(Keyword(kw)), rest
        | Tokenizer.Number(num)::rest -> Some(Number(Int64.Parse(num))), rest
        | Token(sym)::rest -> Some(Symbol(sym)), rest
        | [] -> None, []
        | _ -> raise <| Error.invalidToken ()
        
    let rec readForms acc = function
        | [] -> List.rev acc
        | tokens -> 
            match readForm tokens with
            | Some(form), rest -> readForms (form::acc) rest
            | None, rest -> readForms acc rest

    let read_str str =
        tokenize str |> readForms []
