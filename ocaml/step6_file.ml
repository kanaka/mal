module T = Types.Types

let repl_env = Env.make (Some Core.ns)

let rec eval_ast ast env =
  match ast with
    | T.Symbol s -> Env.get env ast
    | T.List { T.value = xs; T.meta = meta }
      -> T.List { T.value = (List.map (fun x -> eval x env) xs);
                  T.meta = meta }
    | T.Vector { T.value = xs; T.meta = meta }
      -> T.Vector { T.value = (List.map (fun x -> eval x env) xs);
                    T.meta = meta }
    | T.Map { T.value = xs; T.meta = meta }
      -> T.Map {T.meta = meta;
                T.value = (Types.MalMap.fold
                             (fun k v m
                              -> Types.MalMap.add (eval k env) (eval v env) m)
                             xs
                             Types.MalMap.empty)}
    | _ -> ast
and eval ast env =
  match ast with
    | T.List { T.value = [] } -> ast
    | T.List { T.value = [(T.Symbol { T.value = "def!" }); key; expr] } ->
        let value = (eval expr env) in
          Env.set env key value; value
    | T.List { T.value = [(T.Symbol { T.value = "let*" }); (T.Vector { T.value = bindings }); body] }
    | T.List { T.value = [(T.Symbol { T.value = "let*" }); (T.List   { T.value = bindings }); body] } ->
        (let sub_env = Env.make (Some env) in
          let rec bind_pairs = (function
            | sym :: expr :: more ->
                Env.set sub_env sym (eval expr sub_env);
                bind_pairs more
            | _::[] -> raise (Invalid_argument "let* bindings must be an even number of forms")
            | [] -> ())
            in bind_pairs bindings;
          eval body sub_env)
    | T.List { T.value = ((T.Symbol { T.value = "do" }) :: body) } ->
        List.fold_left (fun x expr -> eval expr env) T.Nil body
    | T.List { T.value = [T.Symbol { T.value = "if" }; test; then_expr; else_expr] } ->
        if Types.to_bool (eval test env) then (eval then_expr env) else (eval else_expr env)
    | T.List { T.value = [T.Symbol { T.value = "if" }; test; then_expr] } ->
        if Types.to_bool (eval test env) then (eval then_expr env) else T.Nil
    | T.List { T.value = [T.Symbol { T.value = "fn*" }; T.Vector { T.value = arg_names }; expr] }
    | T.List { T.value = [T.Symbol { T.value = "fn*" }; T.List   { T.value = arg_names }; expr] } ->
        Types.fn
          (function args ->
            let sub_env = Env.make (Some env) in
              let rec bind_args a b =
                (match a, b with
                  | [T.Symbol { T.value = "&" }; name], args -> Env.set sub_env name (Types.list args);
                  | (name :: names), (arg :: args) ->
                      Env.set sub_env name arg;
                      bind_args names args;
                  | [], [] -> ()
                  | _ -> raise (Invalid_argument "Bad param count in fn call"))
              in bind_args arg_names args;
              eval expr sub_env)
    | T.List _ ->
      (match eval_ast ast env with
         | T.List { T.value = ((T.Fn { T.value = f }) :: args) } -> f args
         | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | _ -> eval_ast ast env

let read str = Reader.read_str str
let print exp = Printer.pr_str exp true
let rep str env = print (eval (read str) env)

let rec main =
  try
    Core.init Core.ns;
    Env.set repl_env (Types.symbol "*ARGV*")
            (Types.list (if Array.length Sys.argv > 1
                         then (List.map (fun x -> T.String x) (List.tl (List.tl (Array.to_list Sys.argv))))
                         else []));
    Env.set repl_env (Types.symbol "eval")
            (Types.fn (function [ast] -> eval ast repl_env | _ -> T.Nil));
    let code = "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
    in print_endline code; ignore (rep code repl_env);
    ignore (rep "(def! not (fn* (a) (if a false true)))" repl_env);

    if Array.length Sys.argv > 1 then
      ignore (rep ("(load-file \"" ^ Sys.argv.(1) ^ "\")") repl_env)
    else
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
