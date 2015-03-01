module Reader
    open System
    open Types

    type ParserState = {
        Stack : string list
        Chars : char list
        }

    type Token = string

    type MutableList = System.Collections.Generic.List<Node>

    exception ReaderError of string

    let private accToStr acc =
        String(acc |> List.rev |> Array.ofList)
    
    let rec private skipComment (chars : char list) =
        match chars with
        | '\r'::rest
        | '\n'::rest -> rest
        | ch::rest -> skipComment rest
        | [] -> []

    let private (|IsComment|_|) = function
        | ';'::rest -> Some(skipComment rest)
        | _ -> None

    let rec private skipWhiteSpace (chars : char list) = 
        match chars with
        | ch::rest when Char.IsWhiteSpace(ch) -> skipWhiteSpace rest
        | ','::rest -> skipWhiteSpace rest
        | rest -> rest
    
    let private (|IsWhiteSpace|_|) = function
        | ch::rest when Char.IsWhiteSpace(ch) -> Some(skipWhiteSpace rest)
        | ','::rest -> Some(skipWhiteSpace rest)
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
            | _ -> raise (ReaderError("expected '\"', got EOF"))
        match chars with
        | '"'::rest -> readStrBody ('"'::[]) rest
        | _ -> None

    let private (|IsToken|_|) chars =
        let isTokChar = function
            | '[' | ']' | '{' | '}' | '(' | '\'' | '"' | '`' | ',' | ';' | ')' -> false
            | ch when Char.IsWhiteSpace(ch) -> false
            | _ -> true
        let rec readTokBody acc = function
            | ch::rest when isTokChar ch -> readTokBody (ch::acc) rest
            | rest -> Some(accToStr acc, rest)
        match chars with
        | ch::rest when isTokChar ch -> readTokBody (ch::[]) rest
        | _ -> None

    let rec private getNextToken chars =
        match chars with
        | IsWhiteSpace rest 
        | IsComment rest -> getNextToken rest
        | '~'::'@'::rest -> Some("~@", rest)
        | IsPunct (tok, rest)
        | IsString (tok, rest)
        | IsToken (tok, rest) -> Some(tok, rest)
        | _ -> None

    let rec readTokens chars =
        seq {
            match getNextToken chars with
            | Some(tok, rest)
                -> yield tok
                   yield! readTokens rest
            | _ -> ()
        }

    let private readToken state =
        match state.Stack with
        | tok::rest -> Some(tok), { state with Stack = rest }
        | [] -> match getNextToken state.Chars with
                | Some(tok, rest) -> Some(tok), { Stack = []; Chars = rest }
                | None -> None, { Stack = []; Chars = [] }

    let readNumber str =
        Int64.Parse(str) |> Number

    let readSymbol str =
        str |> Symbol

    let rec readForm (tok : Option<Token>) (state : ParserState) =
        match tok with
        | Some("(") -> let tok, state = readToken state 
                       readList [] tok state
        | Some("[") -> let tok, state = readToken state
                       readVector (MutableList()) tok state
        | Some("{") -> let tok, state = readToken state
                       readMap [] tok state
        | Some("'") -> wrapForm (fun form -> List([Symbol("quote"); form])) state
        | Some("`") -> wrapForm (fun form -> List([Symbol("quasiquote"); form])) state
        | Some("~") -> wrapForm (fun form -> List([Symbol("unquote"); form])) state
        | Some("~@") -> wrapForm (fun form -> List([Symbol("splice-unquote"); form])) state
        | None -> None, state
        | _ -> (readAtom tok, state)

    and wrapForm f state = 
        let tok, state = readToken state
        match readForm tok state with
        | Some(form), state -> Some(f form), state
        | None, _ -> raise (ReaderError("Expected form, got EOF"))

    and readList acc tok state =
        match tok with
        | Some(")") -> Some(List(acc |> List.rev)), state
        | None -> raise (ReaderError("expected ')', got EOF"))
        | _ -> match readForm tok state with
               | Some(form), state ->
                    let tok, state = readToken state
                    readList (form::acc) tok state
               | None, _ -> raise (ReaderError("expected ')', got EOF"))

    and readVector acc tok state =
        match tok with
        | Some("]") -> Some(Vector(acc.ToArray())), state
        | None -> raise (ReaderError("expected ']', got EOF"))
        | _ -> match readForm tok state with
               | Some(form), state ->
                    acc.Add(form)
                    let tok, state = readToken state
                    readVector acc tok state
               | None, _ -> raise (ReaderError("expected ']', got EOF"))

    and readMap acc tok state =
        match tok with
        | Some("}") -> Some(Map(acc |> List.rev |> Map.ofList)), state
        | None -> raise (ReaderError("Expected '}', got EOF"))
        | _ -> match readForm tok state with
               | Some(key), state ->
                    let tok, state = readToken state
                    match readForm tok state with
                    | Some(v), state ->
                        let tok, state = readToken state
                        readMap ((key, v)::acc) tok state
                    | None, _ -> raise (ReaderError("Expected '}', got EOF"))
               | None, _ -> raise (ReaderError("Expected '}', got EOF"))

    and readAtom tok =
        match tok with
        | Some(str) when Char.IsDigit(str.[0]) -> Some(readNumber str)
        | Some(str) -> Some(readSymbol str)
        | None -> None
        
    let read_str str =
        let input = str |> List.ofSeq
        let tok, state = readToken { Stack = []; Chars = input }
        let v, rest = readForm tok state
        v
