module T = Types.Types
        (* ^file ^module *)

let find_re re str =
  List.map (function | Str.Delim x -> x | Str.Text x -> "impossible!")
    (List.filter (function | Str.Delim x -> true | Str.Text x -> false)
      (Str.full_split re str))

let gsub re f str =
  String.concat
    "" (List.map (function | Str.Delim x -> f x | Str.Text x -> x)
                 (Str.full_split re str))

let token_re = (Str.regexp "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"?\\|;.*\\|[^][  \n{}('\"`,;)]*")
let string_re = (Str.regexp "\"\\(\\\\.\\|[^\\\\\"]\\)*\"")

type reader = {
  form : Types.mal_type;
  tokens : string list;
}

type list_reader = {
  list_form : Types.mal_type list;
  tokens : string list;
}

let unescape_string token =
  if Str.string_match string_re token 0
  then
    let without_quotes = String.sub token 1 ((String.length token) - 2) in
    gsub (Str.regexp "\\\\.")
         (function | "\\n" -> "\n" | x -> String.sub x 1 1)
         without_quotes
  else
    (output_string stderr ("expected '\"', got EOF\n");
     flush stderr;
     raise End_of_file)

let read_atom token =
  match token with
    | "nil" -> T.Nil
    | "true" -> T.Bool true
    | "false" -> T.Bool false
    | _ ->
    match token.[0] with
      | '0'..'9' -> T.Int (int_of_string token)
      | '-' -> (match String.length token with
                  | 1 -> T.Symbol token
                  | _ -> (match token.[1] with
                            | '0'..'9' -> T.Int (int_of_string token)
                            | _ -> T.Symbol token))
      | '"' -> T.String (unescape_string token)
      | ':' -> T.Keyword (Str.replace_first (Str.regexp "^:") "" token)
      | _ -> T.Symbol token

let rec read_list eol list_reader =
  match list_reader.tokens with
    | [] -> output_string stderr ("expected '" ^ eol ^ "', got EOF\n");
            flush stderr;
            raise End_of_file;
    | token :: tokens ->
      if Str.string_match (Str.regexp eol) token 0 then
        {list_form = list_reader.list_form; tokens = tokens}
      else if token.[0] = ';' then
        read_list eol { list_form = list_reader.list_form;
                        tokens = tokens }
      else
        let reader = read_form list_reader.tokens in
          read_list eol {list_form = list_reader.list_form @ [reader.form];
                         tokens = reader.tokens}
and read_quote sym tokens =
  let reader = read_form tokens in
    {form = Types.list [ T.Symbol sym; reader.form ];
     tokens = reader.tokens}
and read_form all_tokens =
  match all_tokens with
    | [] -> raise End_of_file;
    | token :: tokens ->
      match token with
        | "'"  -> read_quote "quote" tokens
        | "`"  -> read_quote "quasiquote" tokens
        | "~"  -> read_quote "unquote" tokens
        | "~@" -> read_quote "splice-unquote" tokens
        | "@"  -> read_quote "deref" tokens
        | "^"  ->
           let meta = read_form tokens in
           let value = read_form meta.tokens in
             {form = Types.list [T.Symbol "with-meta"; value.form; meta.form];
              tokens = value.tokens}
        | "(" ->
           let list_reader = read_list ")" {list_form = []; tokens = tokens} in
             {form = Types.list list_reader.list_form;
              tokens = list_reader.tokens}
        | "{" ->
           let list_reader = read_list "}" {list_form = []; tokens = tokens} in
             {form = Types.list_into_map Types.MalMap.empty list_reader.list_form;
              tokens = list_reader.tokens}
        | "[" ->
           let list_reader = read_list "]" {list_form = []; tokens = tokens} in
             {form = Types.vector list_reader.list_form;
              tokens = list_reader.tokens}
        | _ -> if token.[0] = ';'
               then read_form tokens
               else {form = read_atom token; tokens = tokens}

let read_str str = (read_form (List.filter ((<>) "") (find_re token_re str))).form

