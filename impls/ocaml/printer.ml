module T = Types.Types

let rec pr_str mal_obj print_readably =
  let r = print_readably in
    match mal_obj with
      | T.Int i -> string_of_int i
      | T.Symbol s -> s
      | T.Keyword s -> ":" ^ s
      | T.Nil -> "nil"
      | T.Bool true -> "true"
      | T.Bool false -> "false"
      | T.String s ->
          if r
          then  "\"" ^ (Reader.gsub (Str.regexp "\\([\"\\\n]\\)")
                                    (function
                                      | "\n" -> "\\n"
                                      | x -> "\\" ^ x)
                                    s) ^ "\""
          else s
      | T.List { T.value = xs } ->
          "(" ^ (String.concat " " (List.map (fun s -> pr_str s r) xs)) ^ ")"
      | T.Vector { T.value = xs } ->
          "[" ^ (String.concat " " (List.map (fun s -> pr_str s r) xs)) ^ "]"
      | T.Map { T.value = xs } ->
         "{" ^ (Types.MalMap.fold (fun k v s -> s ^ (if s = "" then "" else " ") ^ (pr_str k r)
                                                ^ " " ^ (pr_str v r)) xs "")
         ^ "}"
      | T.Fn f -> "#<fn>"
      | T.Atom x -> "(atom " ^ (pr_str !x r) ^ ")"
