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

  Env.set env (Types.Symbol "list") (Types.Fn (function xs -> Types.MalList xs));
  Env.set env (Types.Symbol "list?")
    (Types.Fn (function [Types.MalList _] -> Types.Bool true | _ -> Types.Bool false));
  Env.set env (Types.Symbol "empty?")
    (Types.Fn (function [Types.MalList []] -> Types.Bool true | _ -> Types.Bool false));
  Env.set env (Types.Symbol "count")
    (Types.Fn (function [Types.MalList xs] -> Types.Int (List.length xs) | _ -> Types.Int 0));
  Env.set env (Types.Symbol "=")
    (Types.Fn (function [a; b] -> Types.Bool (a = b) | _ -> Types.Bool false));

end

