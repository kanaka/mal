open Format

module T = Types.Types

(* Compile the regex once and for all *)
let _pr_escape_re = Str.regexp "\\([\"\\\n]\\)"
let _pr_escape_chunk out = function
  | Str.Text s     -> fprintf out "%s" s
  | Str.Delim "\n" -> fprintf out "\\n"
  | Str.Delim s    -> fprintf out "\\%s" s
let _pr_escape_string out s =
  List.iter (_pr_escape_chunk out) (Str.full_split _pr_escape_re s)

let rec pr_str readably out mal_obj =
    match mal_obj with
      | T.Int i -> fprintf out "%i" i
      | T.Keyword s -> fprintf out ":%s" s
      | T.Nil -> fprintf out "nil"
      | T.Bool b -> fprintf out "%B" b
      | T.String s when readably -> fprintf out "\"%a\"" _pr_escape_string s
      | T.String s | T.Symbol s  -> fprintf out "%s" s
      | T.List { T.value = xs } ->
         fprintf out "(%a)" (pr_list readably true) xs
      | T.Vector { T.value = xs } ->
         fprintf out "[%a]" (pr_list readably true) xs
      | T.Map { T.value = xs } ->
         fprintf out "{%a}" (_pr_map readably) xs
      | T.Fn _ -> fprintf out "#<fn>"
      | T.Atom x -> fprintf out "(atom %a)" (pr_str readably) !x
and pr_list readably spaced out =
  List.iter (
    let sep = ref "" in fun x ->
      fprintf out "%s%a" !sep (pr_str readably) x;
      if spaced && !sep == "" then sep := " " else ())
and _pr_map readably out =
  Types.MalMap.iter (
    let sep = ref "" in fun k v ->
      fprintf out "%s%a %a" !sep (pr_str readably) k (pr_str readably) v;
      if !sep == "" then sep := " " else ())
