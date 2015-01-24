let join sep xs =
  List.fold_left (fun a x -> if a = "" then x else a ^ sep ^ x) "" xs

let rec pr_str mal_obj print_readably =
  match mal_obj with
    | Types.Int i -> string_of_int i
    | Types.Symbol s -> s
    | Types.Keyword s -> ":" ^ s
    | Types.Nil -> "nil"
    | Types.Bool true -> "true"
    | Types.Bool false -> "false"
    | Types.String s ->
        if print_readably
	then  "\"" ^ (Str.global_replace (Str.regexp "\\([\"\\]\\)") "\\\\\\1" s) ^ "\""
        else s
    | Types.List xs ->
        "(" ^ (join " " (List.map (fun s -> pr_str s print_readably) xs)) ^ ")"
    | Types.Fn f -> "#<fn>"
