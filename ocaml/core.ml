module T = Types.Types
let ns = Env.make None

let num_fun t f = Types.fn
  (function
    | [(T.Int a); (T.Int b)] -> t (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let mk_int x = T.Int x
let mk_bool x = T.Bool x

let seq = function
  | T.List   { T.value = xs } -> xs
  | T.Vector { T.value = xs } -> xs
  | T.Map    { T.value = xs } ->
     Types.MalMap.fold (fun k v list -> k :: v :: list) xs []
  | _ -> []

let rec assoc = function
  | c :: k :: v :: (_ :: _ as xs) -> assoc ((assoc [c; k; v]) :: xs)
  | [T.Nil; k; v] -> Types.map (Types.MalMap.add k v Types.MalMap.empty)
  | [T.Map { T.value = m; T.meta = meta }; k; v]
    -> T.Map { T.value = (Types.MalMap.add k v m);
               T.meta = meta }
  | _ -> T.Nil

let rec dissoc = function
  | c :: x :: (_ :: _ as xs) -> dissoc ((dissoc [c; x]) :: xs)
  | [T.Map { T.value = m; T.meta = meta }; k]
    -> T.Map { T.value = (Types.MalMap.remove k m);
               T.meta = meta }
  | _ -> T.Nil

let rec conj = function
  | c :: x :: (_ :: _ as xs) -> conj ((conj [c; x]) :: xs)
  | [T.Map { T.value = c; T.meta = meta }; T.Vector { T.value = [k; v] }]
    -> T.Map { T.value = (Types.MalMap.add k v c);
               T.meta = meta }
  | [T.List { T.value = c; T.meta = meta }; x ]
    -> T.List { T.value = x :: c;
                T.meta = meta }
  | [T.Vector { T.value = c; T.meta = meta }; x ]
    -> T.Vector { T.value = c @ [x];
                  T.meta = meta }
  | _ -> T.Nil

let init env = begin
  Env.set env (Types.symbol "throw")
          (Types.fn (function [ast] -> raise (Types.MalExn ast) | _ -> T.Nil));

  Env.set env (Types.symbol "+")  (num_fun mk_int  ( +  ));
  Env.set env (Types.symbol "-")  (num_fun mk_int  ( -  ));
  Env.set env (Types.symbol "*")  (num_fun mk_int  ( *  ));
  Env.set env (Types.symbol "/")  (num_fun mk_int  ( /  ));
  Env.set env (Types.symbol "<")  (num_fun mk_bool ( <  ));
  Env.set env (Types.symbol "<=") (num_fun mk_bool ( <= ));
  Env.set env (Types.symbol ">")  (num_fun mk_bool ( >  ));
  Env.set env (Types.symbol ">=") (num_fun mk_bool ( >= ));

  Env.set env (Types.symbol "list") (Types.fn (function xs -> Types.list xs));
  Env.set env (Types.symbol "list?")
    (Types.fn (function [T.List _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "vector") (Types.fn (function xs -> Types.vector xs));
  Env.set env (Types.symbol "vector?")
    (Types.fn (function [T.Vector _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "empty?")
    (Types.fn (function
                | [T.List   {T.value = []}] -> T.Bool true
                | [T.Vector {T.value = []}] -> T.Bool true
                | _ -> T.Bool false));
  Env.set env (Types.symbol "count")
    (Types.fn (function
                | [T.List   {T.value = xs}]
                | [T.Vector {T.value = xs}] -> T.Int (List.length xs)
                | _ -> T.Int 0));
  Env.set env (Types.symbol "=")
    (Types.fn (function
                | [T.List a; T.Vector b] -> T.Bool (a = b)
                | [T.Vector a; T.List b] -> T.Bool (a = b)
                | [a; b] -> T.Bool (a = b)
                | _ -> T.Bool false));

  Env.set env (Types.symbol "pr-str")
    (Types.fn (function xs ->
      T.String (String.concat " " (List.map (fun s -> Printer.pr_str s true) xs))));
  Env.set env (Types.symbol "str")
    (Types.fn (function xs ->
      T.String (String.concat "" (List.map (fun s -> Printer.pr_str s false) xs))));
  Env.set env (Types.symbol "prn")
    (Types.fn (function xs ->
      print_endline (String.concat " " (List.map (fun s -> Printer.pr_str s true) xs));
      T.Nil));
  Env.set env (Types.symbol "println")
    (Types.fn (function xs ->
      print_endline (String.concat " " (List.map (fun s -> Printer.pr_str s false) xs));
      T.Nil));

  Env.set env (Types.symbol "compare")
    (Types.fn (function [a; b] -> T.Int (compare a b) | _ -> T.Nil));
  Env.set env (Types.symbol "with-meta")
    (Types.fn (function [a; b] -> Reader.with_meta a b | _ -> T.Nil));
  Env.set env (Types.symbol "meta")
    (Types.fn (function [x] -> Printer.meta x | _ -> T.Nil));

  Env.set env (Types.symbol "read-string")
    (Types.fn (function [T.String x] -> Reader.read_str x | _ -> T.Nil));
  Env.set env (Types.symbol "slurp")
    (Types.fn (function [T.String x] -> T.String (Reader.slurp x) | _ -> T.Nil));

  Env.set env (Types.symbol "cons")
    (Types.fn (function [x; xs] -> Types.list (x :: (seq xs)) | _ -> T.Nil));
  Env.set env (Types.symbol "concat")
    (Types.fn (let rec concat =
                 function
                 | x :: y :: more -> concat ((Types.list ((seq x) @ (seq y))) :: more)
                 | [x] -> x
                 | [] -> Types.list []
               in concat));

  Env.set env (Types.symbol "nth")
    (Types.fn (function [xs; T.Int i] -> List.nth (seq xs) i | _ -> T.Nil));
  Env.set env (Types.symbol "first")
    (Types.fn (function
                | [xs] -> (match seq xs with x :: _ -> x | _ -> T.Nil)
                | _ -> T.Nil));
  Env.set env (Types.symbol "rest")
    (Types.fn (function
                | [xs] -> Types.list (match seq xs with _ :: xs -> xs | _ -> [])
                | _ -> T.Nil));

  Env.set env (Types.symbol "symbol")
    (Types.fn (function [T.String x] -> Types.symbol x | _ -> T.Nil));
  Env.set env (Types.symbol "symbol?")
    (Types.fn (function [T.Symbol _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "keyword")
    (Types.fn (function [T.String x] -> T.Keyword x | _ -> T.Nil));
  Env.set env (Types.symbol "keyword?")
    (Types.fn (function [T.Keyword _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "nil?")
    (Types.fn (function [T.Nil] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "true?")
    (Types.fn (function [T.Bool true] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "false?")
    (Types.fn (function [T.Bool false] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "sequential?")
    (Types.fn (function [T.List _] | [T.Vector _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "apply")
    (Types.fn (function
                | (T.Fn { T.value = f } :: apply_args) ->
                   (match List.rev apply_args with
                    | last_arg :: rev_args ->
                       f ((List.rev rev_args) @ (seq last_arg))
                    | [] -> f [])
                | _ -> raise (Invalid_argument "First arg to apply must be a fn")));
  Env.set env (Types.symbol "map")
    (Types.fn (function
                | [T.Fn { T.value = f }; xs] ->
                   Types.list (List.map (fun x -> f [x]) (seq xs))
                | _ -> T.Nil));
  Env.set env (Types.symbol "readline")
    (Types.fn (function
                | [T.String x] -> print_string x; T.String (read_line ())
                | _ -> T.String (read_line ())));

  Env.set env (Types.symbol "map?")
    (Types.fn (function [T.Map _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "hash-map")
    (Types.fn (function xs -> Types.list_into_map Types.MalMap.empty xs));
  Env.set env (Types.symbol "assoc") (Types.fn assoc);
  Env.set env (Types.symbol "dissoc") (Types.fn dissoc);
  Env.set env (Types.symbol "get")
    (Types.fn (function
                | [T.Map { T.value = m }; k]
                  -> (try Types.MalMap.find k m with _ -> T.Nil)
                | _ -> T.Nil));
  Env.set env (Types.symbol "keys")
    (Types.fn (function
                | [T.Map { T.value = m }]
                  -> Types.list (Types.MalMap.fold (fun k _ c -> k :: c) m [])
                | _ -> T.Nil));
  Env.set env (Types.symbol "vals")
    (Types.fn (function
                | [T.Map { T.value = m }]
                  -> Types.list (Types.MalMap.fold (fun _ v c -> v :: c) m [])
                | _ -> T.Nil));
  Env.set env (Types.symbol "contains?")
    (Types.fn (function
                | [T.Map { T.value = m }; k] -> T.Bool (Types.MalMap.mem k m)
                | _ -> T.Bool false));
  Env.set env (Types.symbol "conj") (Types.fn conj);

  Env.set env (Types.symbol "atom?")
          (Types.fn (function [T.Atom _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "atom")
          (Types.fn (function [x] -> T.Atom (ref x) | _ -> T.Nil));
  Env.set env (Types.symbol "deref")
          (Types.fn (function [T.Atom x] -> !x | _ -> T.Nil));
  Env.set env (Types.symbol "reset!")
          (Types.fn (function [T.Atom x; v] -> x := v; v | _ -> T.Nil));
  Env.set env (Types.symbol "swap!")
          (Types.fn (function T.Atom x :: T.Fn { T.value = f } :: args
                              -> let v = f (!x :: args) in x := v; v | _ -> T.Nil));

  Env.set env (Types.symbol "time-ms")
          (Types.fn (function _ -> T.Int (truncate (1000.0 *. Unix.gettimeofday ()))));
end
