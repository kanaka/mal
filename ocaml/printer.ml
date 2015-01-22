let join sep xs =
  List.fold_left (fun a x -> if a = "" then x else a ^ sep ^ x) "" xs

let rec pr_str mal_obj =
  match mal_obj with
    | Types.Int i -> string_of_int i
    | Types.Symbol s -> s
    | Types.Keyword s -> ":" ^ s
    | Types.Nil -> "nil"
    | Types.Bool true -> "true"
    | Types.Bool false -> "false"
    | Types.String s -> "\""
                        ^ (Str.global_replace (Str.regexp "\"") "\\\"" s)
                        ^ "\""
    | Types.MalList xs -> "(" ^ (join " " (List.map pr_str xs)) ^ ")"
