let repl_env = Env.make (Some Core.ns)

let rec eval_ast ast env =
  match ast with
    | Types.Symbol s -> Env.get env ast
    | Types.MalList xs -> Types.MalList (List.map (fun x -> eval x env) xs)
    | _ -> ast
and eval ast env =
  match ast with
    | Types.MalList [(Types.Symbol "def!"); key; expr] ->
        let value = (eval expr env) in
          Env.set env key value; value
    | Types.MalList [(Types.Symbol "let*"); (Types.MalList bindings); body] ->
        (let sub_env = Env.make (Some env) in
          let rec bind_pairs = (function
            | sym :: expr :: more ->
                Env.set sub_env sym (eval expr sub_env);
                bind_pairs more
            | _::[] -> raise (Invalid_argument "let* bindings must be an even number of forms")
            | [] -> ())
            in bind_pairs bindings;
          eval body sub_env)
    | Types.MalList ((Types.Symbol "do") :: body) ->
        List.fold_left (fun x expr -> eval expr env) Types.Nil body
    | Types.MalList [Types.Symbol "if"; test; then_expr; else_expr] ->
        if Types.to_bool (eval test env) then (eval then_expr env) else (eval else_expr env)
    | Types.MalList [Types.Symbol "if"; test; then_expr] ->
        if Types.to_bool (eval test env) then (eval then_expr env) else Types.Nil
    | Types.MalList [Types.Symbol "fn*"; Types.MalList arg_names; expr] ->
        Types.Fn
          (function args ->
            let sub_env = Env.make (Some env) in
              let rec bind_args = (fun a b ->
                (match a, b with
                  | [Types.Symbol "&"; name], args -> Env.set sub_env name (Types.MalList args);
                  | (name :: names), (arg :: args) ->
                      Env.set sub_env name arg;
                      bind_args names args;
                  | [], [] -> ()
                  | _ -> raise (Invalid_argument "Bad param count in fn call")))
              in (bind_args arg_names args);
              eval expr sub_env)
    | Types.MalList _ ->
      (match eval_ast ast env with
         | Types.MalList ((Types.Fn f) :: args) -> f args
         | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | _ -> eval_ast ast env

let read str = Reader.read_str str
let print exp = Printer.pr_str exp
let rep str env = print (eval (read str) env)

let rec main =
  try
    Core.init Core.ns;
    ignore (rep "(def! not (fn* (a) (if a false true)))" repl_env);
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
