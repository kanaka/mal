module T = Types.Types
let ns = Env.make None

let num_fun t f = T.Fn
  (function
    | [(T.Int a); (T.Int b)] -> t (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let mk_int x = T.Int x
let mk_bool x = T.Bool x

let init env = begin
  Env.set env (Types.symbol "+")  (num_fun mk_int  ( +  ));
  Env.set env (Types.symbol "-")  (num_fun mk_int  ( -  ));
  Env.set env (Types.symbol "*")  (num_fun mk_int  ( *  ));
  Env.set env (Types.symbol "/")  (num_fun mk_int  ( /  ));
  Env.set env (Types.symbol "<")  (num_fun mk_bool ( <  ));
  Env.set env (Types.symbol "<=") (num_fun mk_bool ( <= ));
  Env.set env (Types.symbol ">")  (num_fun mk_bool ( >  ));
  Env.set env (Types.symbol ">=") (num_fun mk_bool ( >= ));

  Env.set env (Types.symbol "list") (T.Fn (function xs -> Types.list xs));
  Env.set env (Types.symbol "list?")
    (T.Fn (function [T.List _] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "empty?")
    (T.Fn (function [T.List {T.value = []}] -> T.Bool true | _ -> T.Bool false));
  Env.set env (Types.symbol "count")
    (T.Fn (function [T.List {T.value = xs}] -> T.Int (List.length xs) | _ -> T.Int 0));
  Env.set env (Types.symbol "=")
    (T.Fn (function
            | [T.List a; T.Vector b] -> T.Bool (a = b)
            | [T.Vector a; T.List b] -> T.Bool (a = b)
            | [a; b] -> T.Bool (a = b)
            | _ -> T.Bool false));

  Env.set env (Types.symbol "pr-str")
    (T.Fn (function xs ->
      T.String (Printer.join " " (List.map (fun s -> Printer.pr_str s true) xs))));
  Env.set env (Types.symbol "str")
    (T.Fn (function xs ->
      T.String (Printer.join "" (List.map (fun s -> Printer.pr_str s false) xs))));
  Env.set env (Types.symbol "prn")
    (T.Fn (function xs ->
      print_endline (Printer.join " " (List.map (fun s -> Printer.pr_str s true) xs));
      T.Nil));
  Env.set env (Types.symbol "println")
    (T.Fn (function xs ->
      print_endline (Printer.join " " (List.map (fun s -> Printer.pr_str s false) xs));
      T.Nil));

  Env.set env (Types.symbol "compare")
    (T.Fn (function [a; b] -> T.Int (compare a b) | _ -> T.Nil));
  Env.set env (Types.symbol "with-meta")
    (T.Fn (function [a; b] -> Reader.with_meta a b | _ -> T.Nil));
  Env.set env (Types.symbol "meta")
    (T.Fn (function [x] -> Printer.meta x | _ -> T.Nil));
end

