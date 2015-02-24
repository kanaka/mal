module Reader
    open System
    open Types

    let private accToStr acc =
        String(acc |> List.rev |> Array.ofList)

    let rec private WhileWhiteSpace (chars : char list) = 
        match chars with
        | ch::rest when Char.IsWhiteSpace(ch) -> WhileWhiteSpace rest
        | rest -> rest

    let private (|IsWhiteSpace|_|) = function
        | ch::rest when Char.IsWhiteSpace(ch) -> Some(WhileWhiteSpace rest)
        | _ -> None

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
        
