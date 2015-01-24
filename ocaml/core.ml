let ns = Env.make None

let num_fun t f = Types.Fn
  (function
    | [(Types.Int a); (Types.Int b)] -> t (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let mk_int x = Types.Int x
let mk_bool x = Types.Bool x

let init env = begin
  Env.set env (Types.Symbol "+")  (num_fun mk_int  ( +  ));
  Env.set env (Types.Symbol "-")  (num_fun mk_int  ( -  ));
  Env.set env (Types.Symbol "*")  (num_fun mk_int  ( *  ));
  Env.set env (Types.Symbol "/")  (num_fun mk_int  ( /  ));
  Env.set env (Types.Symbol "<")  (num_fun mk_bool ( <  ));
  Env.set env (Types.Symbol "<=") (num_fun mk_bool ( <= ));
  Env.set env (Types.Symbol ">")  (num_fun mk_bool ( >  ));
  Env.set env (Types.Symbol ">=") (num_fun mk_bool ( >= ));

  Env.set env (Types.Symbol "list") (Types.Fn (function xs -> Types.List xs));
  Env.set env (Types.Symbol "list?")
    (Types.Fn (function [Types.List _] -> Types.Bool true | _ -> Types.Bool false));
  Env.set env (Types.Symbol "empty?")
    (Types.Fn (function [Types.List []] -> Types.Bool true | _ -> Types.Bool false));
  Env.set env (Types.Symbol "count")
    (Types.Fn (function [Types.List xs] -> Types.Int (List.length xs) | _ -> Types.Int 0));
  Env.set env (Types.Symbol "=")
    (Types.Fn (function [a; b] -> Types.Bool (a = b) | _ -> Types.Bool false));

  Env.set env (Types.Symbol "pr-str")
    (Types.Fn (function xs ->
      Types.String (Printer.join " " (List.map (fun s -> Printer.pr_str s true) xs))));
  Env.set env (Types.Symbol "str")
    (Types.Fn (function xs ->
      Types.String (Printer.join "" (List.map (fun s -> Printer.pr_str s false) xs))));
  Env.set env (Types.Symbol "prn")
    (Types.Fn (function xs ->
      print_endline (Printer.join " " (List.map (fun s -> Printer.pr_str s true) xs));
      Types.Nil));
  Env.set env (Types.Symbol "println")
    (Types.Fn (function xs ->
      print_endline (Printer.join " " (List.map (fun s -> Printer.pr_str s false) xs));
      Types.Nil));
end

