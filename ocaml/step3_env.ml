let num_fun f = Types.Fn
  (function
    | [(Types.Int a); (Types.Int b)] -> Types.Int (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let repl_env = Env.make None

let init_repl env = begin
  Env.set env (Types.Symbol "+") (num_fun ( + ));
  Env.set env (Types.Symbol "-") (num_fun ( - ));
  Env.set env (Types.Symbol "*") (num_fun ( * ));
  Env.set env (Types.Symbol "/") (num_fun ( / ));
end

let rec eval_ast ast env =
  match ast with
    | Types.Symbol s -> Env.get env ast
    | Types.List xs -> Types.List (List.map (fun x -> eval x env) xs)
    | _ -> ast
and eval ast env =
  match ast with
    | Types.List [(Types.Symbol "def!"); key; expr] ->
        let value = (eval expr env) in
          Env.set env key value; value
    | Types.List [(Types.Symbol "let*"); (Types.List bindings); body] ->
        (let sub_env = Env.make (Some env) in
          let rec bind_pairs = (function
            | sym :: expr :: more ->
                Env.set sub_env sym (eval expr sub_env);
                bind_pairs more
            | _::[] -> raise (Invalid_argument "let* bindings must be an even number of forms")
            | [] -> ())
            in bind_pairs bindings;
          eval body sub_env)
    | Types.List _ ->
      (match eval_ast ast env with
         | Types.List ((Types.Fn f) :: args) -> f args
         | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | _ -> eval_ast ast env

let read str = Reader.read_str str
let print exp = Printer.pr_str exp true
let rep str env = print (eval (read str) env)

let rec main =
  try
    init_repl repl_env;
    while true do
      print_string "user> ";
      let line = read_line () in
        try
          print_endline (rep line repl_env);
        with End_of_file -> ()
         | Invalid_argument x ->
             output_string stderr ("Invalid_argument exception: " ^ x ^ "\n");
             flush stderr
    done
  with End_of_file -> ()
