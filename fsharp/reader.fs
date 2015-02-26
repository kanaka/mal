module Reader
    open System
    open Types

    let private accToStr acc =
        String(acc |> List.rev |> Array.ofList)
    
    let rec private whileWhiteSpace (chars : char list) = 
        match chars with
        | ch::rest when Char.IsWhiteSpace(ch) -> whileWhiteSpace rest
        | rest -> rest
    
    let private (|IsWhiteSpace|_|) = function
        | ch::rest when Char.IsWhiteSpace(ch) -> Some(whileWhiteSpace rest)
        | _ -> None

    let private (|IsPunct|_|) = function
        | '['::rest -> Some("[", rest)
        | ']'::rest -> Some("]", rest)
        | '{'::rest -> Some("{", rest)
        | '}'::rest -> Some("}", rest)
        | '('::rest -> Some("(", rest)
        | ')'::rest -> Some(")", rest)
        | '\''::rest -> Some("\'", rest)
        | '`'::rest -> Some("`", rest)
        | '~'::rest -> Some("~", rest)
        | '^'::rest -> Some("^", rest)
        | '@'::rest -> Some("@", rest)
        | _ -> None

    let private (|IsString|_|) (chars : char list) =
        let rec readStrBody (acc : char list) (chars : char list) =
            match chars with
            | '\\'::ch::rest -> readStrBody (ch::'\\'::acc) rest
            | '"'::rest -> Some(accToStr ('"'::acc), rest)
            | '\\'::rest -> None // throw exception here?
            | ch::rest -> readStrBody (ch::acc) rest
            | _ -> None // throw exception here?

        match chars with
        | '"'::rest -> readStrBody ('"'::[]) rest
        | _ -> None 

    let rec private getNextToken chars =
        match chars with
        | IsWhiteSpace rest -> getNextToken rest
        | '~'::'@'::rest -> Some("~@", rest)
        | IsPunct (tok, rest) -> Some(tok, rest)
        | IsString (tok, rest) -> Some(tok, rest)
        | _ -> None

    let rec readTokens chars =
        seq {
            match getNextToken chars with
            | Some(tok, rest)
                -> yield tok
                   yield! readTokens rest
            | _ -> ()
        }

    let rec private WhileInt64 acc (chars : char list) =
        match chars with
        | ch::rest when Char.IsDigit(ch) -> WhileInt64 (ch::acc) rest
        | ch::rest -> acc |> accToStr, ch::rest
        | [] -> acc |> accToStr, []

    let private (|IsNumber|_|) = function
        | ch::rest when Char.IsDigit(ch) 
            -> let token, rest = WhileInt64 (ch::[]) rest
               let v = Int64.Parse(token)
               Some(Number(v), rest)
        | _ -> None

    let rec private read_form = function
        | IsWhiteSpace rest -> read_form rest
        | IsNumber (num, rest) -> num, rest
        | _ -> List([]), [] // TODO: Need to add more cases

    let read_str str =
        let input = str |> List.ofSeq
        let output, rest = read_form input
        output
        
