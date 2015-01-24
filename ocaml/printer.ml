let join sep xs =
  List.fold_left (fun a x -> if a = "" then x else a ^ sep ^ x) "" xs

let rec pr_pairs xs str print_readably = match xs with
  | k :: v :: more -> pr_pairs more ((if str = "" then str else (str ^ ", "))
                                     ^ (pr_str k print_readably)
                                     ^ " "
                                     ^ (pr_str v print_readably))
                               print_readably
  | _ :: [] -> raise (Invalid_argument "Partition requires even number of items")
  | [] -> str

and pr_str mal_obj print_readably =
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
    | Types.Vector xs ->
        "[" ^ (join " " (List.map (fun s -> pr_str s print_readably) xs)) ^ "]"
    | Types.Map xs -> "{" ^ pr_pairs xs "" print_readably ^ "}"
    | Types.Fn f -> "#<fn>"
