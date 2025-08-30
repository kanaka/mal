module T = Types.Types

let ns = Env.make None
let kw_macro = T.Keyword "macro"

let num_fun t f =
  Types.fn (function
    | [ T.Int a; T.Int b ] -> t (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let mk_int x = T.Int x
let mk_bool x = T.Bool x

let rec mal_equal a b =
  match (a, b) with
  | T.List { T.value = xs }, T.List { T.value = ys }
  | T.List { T.value = xs }, T.Vector { T.value = ys }
  | T.Vector { T.value = xs }, T.List { T.value = ys }
  | T.Vector { T.value = xs }, T.Vector { T.value = ys } ->
      List.equal mal_equal xs ys
  | T.Map { T.value = xs }, T.Map { T.value = ys } ->
      Types.MalMap.equal mal_equal xs ys
  | _ -> a = b

let seq = function
  | T.List { T.value = xs } -> xs
  | T.Vector { T.value = xs } -> xs
  | _ -> []

let mal_seq = function
  | [ (T.List { T.value = xs } as lst) ] when not (List.is_empty xs) -> lst
  | [ T.Vector { T.value = xs } ] when not (List.is_empty xs) -> Types.list xs
  | [ T.String s ] when 0 < String.length s ->
      Types.list (List.map (fun x -> T.String x) (Str.split (Str.regexp "") s))
  | _ -> T.Nil

let rec assoc = function
  | T.Map { T.value = m } :: xs -> Types.list_into_map m xs
  | _ -> T.Nil

let rec dissoc = function
  | T.Map { T.value = m } :: xs ->
      Types.map (List.fold_left (fun k m -> Types.MalMap.remove m k) m xs)
  | _ -> T.Nil

let rec conj = function
  | c :: x :: (_ :: _ as xs) -> conj (conj [ c; x ] :: xs)
  | [ T.List { T.value = c; T.meta }; x ] -> T.List { T.value = x :: c; T.meta }
  | [ T.Vector { T.value = c; T.meta }; x ] ->
      T.Vector { T.value = c @ [ x ]; T.meta }
  | _ -> T.Nil

let init env =
  Env.set env "throw"
    (Types.fn (function [ ast ] -> raise (Types.MalExn ast) | _ -> T.Nil));

  Env.set env "+" (num_fun mk_int ( + ));
  Env.set env "-" (num_fun mk_int ( - ));
  Env.set env "*" (num_fun mk_int ( * ));
  Env.set env "/" (num_fun mk_int ( / ));
  Env.set env "<" (num_fun mk_bool ( < ));
  Env.set env "<=" (num_fun mk_bool ( <= ));
  Env.set env ">" (num_fun mk_bool ( > ));
  Env.set env ">=" (num_fun mk_bool ( >= ));

  Env.set env "list" (Types.fn (function xs -> Types.list xs));
  Env.set env "list?"
    (Types.fn (function [ T.List _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "vector" (Types.fn (function xs -> Types.vector xs));
  Env.set env "vector?"
    (Types.fn (function [ T.Vector _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "empty?"
    (Types.fn (function
      | [ T.List { T.value = [] } ] -> T.Bool true
      | [ T.Vector { T.value = [] } ] -> T.Bool true
      | _ -> T.Bool false));
  Env.set env "count"
    (Types.fn (function
      | [ T.List { T.value = xs } ] | [ T.Vector { T.value = xs } ] ->
          T.Int (List.length xs)
      | _ -> T.Int 0));
  Env.set env "="
    (Types.fn (function
      | [ a; b ] -> T.Bool (mal_equal a b)
      | _ -> T.Bool false));

  Env.set env "pr-str"
    (Types.fn (function xs ->
        T.String (Format.asprintf "%a" (Printer.pr_list true true) xs)));
  Env.set env "str"
    (Types.fn (function xs ->
        T.String (Format.asprintf "%a" (Printer.pr_list false false) xs)));
  Env.set env "prn"
    (Types.fn (function xs ->
        Format.printf "%a\n" (Printer.pr_list true true) xs;
        T.Nil));
  Env.set env "println"
    (Types.fn (function xs ->
        Format.printf "%a\n" (Printer.pr_list false true) xs;
        T.Nil));

  Env.set env "compare"
    (Types.fn (function [ a; b ] -> T.Int (compare a b) | _ -> T.Nil));
  Env.set env "with-meta"
    (Types.fn (function
      | [ T.List v; m ] -> T.List { v with T.meta = m }
      | [ T.Map v; m ] -> T.Map { v with T.meta = m }
      | [ T.Vector v; m ] -> T.Vector { v with T.meta = m }
      | [ T.Fn v; m ] -> T.Fn { v with meta = m }
      | _ -> T.Nil));
  Env.set env "meta"
    (Types.fn (function
      | [ T.List { T.meta } ] -> meta
      | [ T.Map { T.meta } ] -> meta
      | [ T.Vector { T.meta } ] -> meta
      | [ T.Fn { meta } ] -> meta
      | _ -> T.Nil));

  Env.set env "read-string"
    (Types.fn (function [ T.String x ] -> Reader.read_str x | _ -> T.Nil));
  Env.set env "slurp"
    (Types.fn (function
      | [ T.String x ] ->
          let chan = open_in x in
          let b = Buffer.create 27 in
          Buffer.add_channel b chan (in_channel_length chan);
          close_in chan;
          T.String (Buffer.contents b)
      | _ -> T.Nil));

  Env.set env "cons"
    (Types.fn (function [ x; xs ] -> Types.list (x :: seq xs) | _ -> T.Nil));
  Env.set env "concat"
    (Types.fn
       (let rec concat = function
          | x :: y :: more -> concat (Types.list (seq x @ seq y) :: more)
          | [ (T.List _ as x) ] -> x
          | [ x ] -> Types.list (seq x)
          | [] -> Types.list []
        in
        concat));
  Env.set env "vec"
    (Types.fn (function
      | [ T.List { T.value = xs } ] -> Types.vector xs
      | [ T.Vector { T.value = xs } ] -> Types.vector xs
      | [ _ ] -> raise (Invalid_argument "vec: expects a sequence")
      | _ -> raise (Invalid_argument "vec: arg count")));

  Env.set env "nth"
    (Types.fn (function
      | [ xs; T.Int i ] -> (
          try List.nth (seq xs) i
          with _ -> raise (Invalid_argument "nth: index out of range"))
      | _ -> T.Nil));
  Env.set env "first"
    (Types.fn (function
      | [ xs ] -> ( match seq xs with x :: _ -> x | _ -> T.Nil)
      | _ -> T.Nil));
  Env.set env "rest"
    (Types.fn (function
      | [ xs ] -> Types.list (match seq xs with _ :: xs -> xs | _ -> [])
      | _ -> T.Nil));

  Env.set env "string?"
    (Types.fn (function [ T.String _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "symbol"
    (Types.fn (function [ T.String x ] -> T.Symbol x | _ -> T.Nil));
  Env.set env "symbol?"
    (Types.fn (function [ T.Symbol _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "keyword"
    (Types.fn (function
      | [ T.String x ] -> T.Keyword x
      | [ T.Keyword x ] -> T.Keyword x
      | _ -> T.Nil));
  Env.set env "keyword?"
    (Types.fn (function [ T.Keyword _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "number?"
    (Types.fn (function [ T.Int _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "fn?"
    (Types.fn (function
      | [ T.Fn { macro = false } ] -> T.Bool true
      | _ -> T.Bool false));
  Env.set env "macro?"
    (Types.fn (function
      | [ T.Fn { macro = true } ] -> T.Bool true
      | _ -> T.Bool false));
  Env.set env "nil?"
    (Types.fn (function [ T.Nil ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "true?"
    (Types.fn (function [ T.Bool true ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "false?"
    (Types.fn (function [ T.Bool false ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "sequential?"
    (Types.fn (function
      | [ T.List _ ] | [ T.Vector _ ] -> T.Bool true
      | _ -> T.Bool false));
  Env.set env "apply"
    (Types.fn (function
      | T.Fn { value = f } :: apply_args -> (
          match List.rev apply_args with
          | last_arg :: rev_args -> f (List.rev rev_args @ seq last_arg)
          | [] -> f [])
      | _ -> raise (Invalid_argument "First arg to apply must be a fn")));
  Env.set env "map"
    (Types.fn (function
      | [ T.Fn { value = f }; xs ] ->
          Types.list (List.map (fun x -> f [ x ]) (seq xs))
      | _ -> T.Nil));
  Env.set env "readline"
    (Types.fn (function
      | [ T.String x ] ->
          Format.printf "%s%!" x;
          T.String (read_line ())
      | _ -> T.String (read_line ())));

  Env.set env "map?"
    (Types.fn (function [ T.Map _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "hash-map" (Types.fn (Types.list_into_map Types.MalMap.empty));
  Env.set env "assoc" (Types.fn assoc);
  Env.set env "dissoc" (Types.fn dissoc);
  Env.set env "get"
    (Types.fn (function
      | [ T.Map { T.value = m }; k ] -> (
          try Types.MalMap.find k m with _ -> T.Nil)
      | _ -> T.Nil));
  Env.set env "keys"
    (Types.fn (function
      | [ T.Map { T.value = m } ] ->
          Types.list (Types.MalMap.fold (fun k _ c -> k :: c) m [])
      | _ -> T.Nil));
  Env.set env "vals"
    (Types.fn (function
      | [ T.Map { T.value = m } ] ->
          Types.list (Types.MalMap.fold (fun _ v c -> v :: c) m [])
      | _ -> T.Nil));
  Env.set env "contains?"
    (Types.fn (function
      | [ T.Map { T.value = m }; k ] -> T.Bool (Types.MalMap.mem k m)
      | _ -> T.Bool false));
  Env.set env "conj" (Types.fn conj);
  Env.set env "seq" (Types.fn mal_seq);

  Env.set env "atom?"
    (Types.fn (function [ T.Atom _ ] -> T.Bool true | _ -> T.Bool false));
  Env.set env "atom"
    (Types.fn (function [ x ] -> T.Atom (ref x) | _ -> T.Nil));
  Env.set env "deref" (Types.fn (function [ T.Atom x ] -> !x | _ -> T.Nil));
  Env.set env "reset!"
    (Types.fn (function
      | [ T.Atom x; v ] ->
          x := v;
          v
      | _ -> T.Nil));
  Env.set env "swap!"
    (Types.fn (function
      | T.Atom x :: T.Fn { value = f } :: args ->
          let v = f (!x :: args) in
          x := v;
          v
      | _ -> T.Nil));

  Env.set env "time-ms"
    (Types.fn (function _ -> T.Int (truncate (1000.0 *. Unix.gettimeofday ()))))
