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

let init env = begin
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
  Env.set env (Types.symbol "empty?")
    (Types.fn (function [T.List {T.value = []}] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "count")
    (Types.fn (function [T.List {T.value = xs}] -> T.Int (List.length xs) | _ -> T.Int 0));
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
end

