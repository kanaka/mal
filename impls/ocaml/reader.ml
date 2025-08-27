open Str (* not reentrant, but simple and always available *)
open Types

let separator_re = regexp "\\([, \t\n]\\|;[^\n]*\\)+"
let number_re = regexp "-?[0-9]+"
let chars = "[^][, \t\n;(){}'`~@^\"]+"
let keyword_re = regexp (":\\(" ^ chars ^ "\\)")
let symbol_re = regexp chars
let string_re = regexp {|"\(\(\\[\\n"]\|[^\\"]\)*\)"|}
let escape_re = regexp {|\\.|}
let quote_re = regexp_string "'"
let quasiquote_re = regexp_string "`"
let deref_re = regexp_string "@"
let unquote_re = regexp_string "~"
let sp_unq_re = regexp_string "~@"
let with_meta_re = regexp_string "^"
let list_re = regexp_string "("
let map_re = regexp_string "{"
let vector_re = regexp_string "["
let close_re = regexp "[])}]" (* so "[1 2)" is accepted as a vector *)

let unescape str =
  let e = match_end () - 1 in
  if str.[e] == 'n' then "\n" else String.sub str e 1

let read_str str =
  (* !p is the currently parsed position inside str *)
  let rec read pattern p =
    let result = string_match pattern str !p in
    if result then p := match_end ();
    result
  and read_list p =
    ignore (read separator_re p);
    if read close_re p then []
    else
      (* Parse the first form before the rest of the list *)
      let first = read_form p in
      first :: read_list p
  and read_form p =
    ignore (read separator_re p);
    if read number_re p then Types.Int (int_of_string (matched_string str))
    else if read keyword_re p then Keyword (matched_group 1 str)
    else if read symbol_re p then
      match matched_string str with
      | "nil" -> Nil
      | "true" -> Bool true
      | "false" -> Bool false
      | t -> Symbol t
    else if read string_re p then
      String (global_substitute escape_re unescape (matched_group 1 str))
    else if read quote_re p then list [ Symbol "quote"; read_form p ]
    else if read quasiquote_re p then list [ Symbol "quasiquote"; read_form p ]
    else if read deref_re p then list [ Symbol "deref"; read_form p ]
    else if read sp_unq_re p then list [ Symbol "splice-unquote"; read_form p ]
    else if read unquote_re p then list [ Symbol "unquote"; read_form p ]
    else if read with_meta_re p then
      (* Parse the metadata before the value *)
      let meta = read_form p in
      list [ Symbol "with-meta"; read_form p; meta ]
    else if read list_re p then list (read_list p)
    else if read vector_re p then vector (read_list p)
    else if read map_re p then list_into_map MalMap.empty (read_list p)
    else raise (Invalid_argument "unexpected EOF ] } ) or string escape")
  in
  read_form (ref 0)
