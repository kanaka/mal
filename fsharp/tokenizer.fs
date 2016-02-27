module Tokenizer

    open System
    open Types
    
    type Token =
        | EOF
        | OpenBracket | CloseBracket
        | OpenBrace | CloseBrace
        | OpenParen | CloseParen
        | SingleQuote
        | Backtick
        | Tilde | SpliceUnquote
        | Caret
        | At
        | String of string
        | Token of string
        | Keyword of string
        | Number of string


    let tokenize (str : string) =
        let len = str.Length
        
        let inline isWhiteSpace ch = ch = ',' || Char.IsWhiteSpace(ch)
        let inline isNotNewline ch = ch <> '\r' && ch <> '\n'
        let inline isDigit ch = Char.IsDigit(ch)
        let inline isTokenChar ch =
            match ch with
            | '[' | ']' | '{' | '}' | '(' | ')'
            | '\'' | '"' | '`' | ',' | ';' -> false
            | ch when Char.IsWhiteSpace(ch) -> false
            | _ -> true
       
        let rec skipWhile pred p =
            if p >= len then p
            elif pred (str.[p]) then p + 1 |> skipWhile pred
            else p

        let rec accumulateWhile pred (f : string -> Token) start p =
            if p >= len then str.Substring(start, p - start) |> f, p
            elif pred (str.[p]) then p + 1 |> accumulateWhile pred f start
            else str.Substring(start, p - start) |> f, p

        let accumulateString p =
            let b = System.Text.StringBuilder()
            let rec accChar (ch : char) n =
                b.Append(ch) |> ignore
                accChars n
            and accChars p =
                let n = p + 1
                if p >= len then raise <| Error.expectedXButEOF "'\"'"
                match str.[p] with
                | '\\' -> accEscaped n
                | '"' -> n
                | ch -> accChar ch n
            and accEscaped p =
                let n = p + 1
                if p >= len then raise <| Error.expectedXButEOF "char"
                match str.[p] with
                | 't'  -> accChar '\t' n
                | 'b'  -> accChar '\b' n
                | 'n'  -> accChar '\n' n
                | 'r'  -> accChar '\r' n
                | 'f'  -> accChar '\f' n
                | '\'' -> accChar '\'' n
                | '"'  -> accChar '"' n
                | '\\' -> accChar '\\' n
                | _ -> raise <| Error.expectedXButEOF "valid escape char"
            let n = accChars p
            String(b.ToString()), n

        let accumulateKeyword p =
            let n = p + 1
            if p >= len then raise <| Error.expectedXButEOF "keyword"
            elif isTokenChar str.[p] then accumulateWhile isTokenChar Keyword p n
            else raise <| Error.expectedX "keyword char"

        let accumulateSpliceUnquote p =
            if p >= len then Tilde, p
            elif str.[p] = '@' then SpliceUnquote, (p + 1)
            else Tilde, p

        let rec getToken p =
            if p >= len then
                EOF, p
            else
                let n = p + 1
                match str.[p] with
                | ch when isWhiteSpace ch -> getToken n
                | ';' -> skipWhile isNotNewline n |> getToken
                | '[' -> OpenBracket, n
                | ']' -> CloseBracket, n
                | '{' -> OpenBrace, n
                | '}' -> CloseBrace, n
                | '(' -> OpenParen, n
                | ')' -> CloseParen, n
                | '\'' -> SingleQuote, n
                | '`' -> Backtick, n
                | '~' -> accumulateSpliceUnquote n
                | '^' -> Caret, n
                | '@' -> At, n
                | '"' -> accumulateString n
                | ':' -> accumulateKeyword n
                | '-' when isDigit str.[n] -> accumulateWhile isDigit Number p n
                | ch when isDigit ch -> accumulateWhile isDigit Number p n
                | ch when isTokenChar ch -> accumulateWhile isTokenChar Token p n
                | _ -> raise <| Error.unexpectedChar ()

        let rec accumulate acc p = 
            match getToken p with
            | EOF, p -> List.rev acc
            | tok, p -> accumulate (tok::acc) p

        accumulate [] 0
