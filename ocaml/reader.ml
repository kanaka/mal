module T = Types.Types
        (* ^file ^module *)

let slurp filename =
  let chan = open_in filename in
  let b = Buffer.create 27 in
  Buffer.add_channel b chan (in_channel_length chan) ;
  close_in chan ;
  Buffer.contents b

let find_re re str =
  List.map (function | Str.Delim x -> x | Str.Text x -> "impossible!")
    (List.filter (function | Str.Delim x -> true | Str.Text x -> false)
      (Str.full_split re str))

let gsub re f str =
  String.concat
    "" (List.map (function | Str.Delim x -> f x | Str.Text x -> x)
                 (Str.full_split re str))

let token_re = (Str.regexp "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"\\|;.*\\|[^][  \n{}('\"`,;)]*")

type reader = {
  form : Types.mal_type;
  tokens : string list;
}

type list_reader = {
  list_form : Types.mal_type list;
  tokens : string list;
}

let read_atom token =
  match token with
    | "nil" -> T.Nil
    | "true" -> T.Bool true
    | "false" -> T.Bool false
    | _ ->
    match token.[0] with
      | '0'..'9' -> T.Int (int_of_string token)
      | '-' -> (match String.length token with
                  | 1 -> Types.symbol token
                  | _ -> (match token.[1] with
                            | '0'..'9' -> T.Int (int_of_string token)
                            | _ -> Types.symbol token))
      | '"' -> T.String (gsub (Str.regexp "\\\\.")
                              (function
                                | "\\n" -> "\n"
                                | x -> String.sub x 1 1)
                              (String.sub token 1 ((String.length token) - 2)))
      | ':' -> T.Keyword (Str.replace_first (Str.regexp "^:") "" token)
      | _ -> Types.symbol token

let with_meta obj meta =
  match obj with
    | T.List   { T.value = v }
      -> T.List   { T.value = v; T.meta = meta }; | T.Map    { T.value = v }
      -> T.Map    { T.value = v; T.meta = meta }; | T.Vector { T.value = v }
      -> T.Vector { T.value = v; T.meta = meta }; | T.Symbol { T.value = v }
      -> T.Symbol { T.value = v; T.meta = meta }; | T.Fn     { T.value = v }
      -> T.Fn     { T.value = v; T.meta = meta };
    | _ -> raise (Invalid_argument "metadata not supported on this type")

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
    {form = Types.list [ Types.symbol sym; reader.form ];
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
             {(*form = with_meta value.form meta.form;*)
              form = Types.list [Types.symbol "with-meta"; value.form; meta.form];
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

