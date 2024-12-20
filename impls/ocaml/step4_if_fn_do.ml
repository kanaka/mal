module T = Types.Types

let repl_env = Env.make (Some Core.ns)

let rec eval ast env =
  (match Env.get env "DEBUG-EVAL" with
    | None                -> ()
    | Some T.Nil          -> ()
    | Some (T.Bool false) -> ()
    | Some _              ->
      output_string stderr ("EVAL: " ^ (Printer.pr_str ast true) ^ "\n");
      flush stderr);
  match ast with
    | T.Symbol s -> (match Env.get env s with
         | Some v -> v
         | None   -> raise (Invalid_argument ("'" ^ s ^ "' not found")))
    | T.Vector { T.value = xs; T.meta = meta }
      -> T.Vector { T.value = (List.map (fun x -> eval x env) xs);
                    T.meta = meta }
    | T.Map { T.value = xs; T.meta = meta }
      -> T.Map {T.meta = meta;
                T.value = (Types.MalMap.fold
                             (fun k v m
                              -> Types.MalMap.add k (eval v env) m)
                             xs
                             Types.MalMap.empty)}
    | T.List { T.value = [T.Symbol "def!"; T.Symbol key; expr] } ->
        let value = (eval expr env) in
          Env.set env key value; value
    | T.List { T.value = [T.Symbol "let*"; (T.Vector { T.value = bindings }); body] }
    | T.List { T.value = [T.Symbol "let*"; (T.List   { T.value = bindings }); body] } ->
        (let sub_env = Env.make (Some env) in
          let rec bind_pairs = (function
            | T.Symbol sym :: expr :: more ->
                Env.set sub_env sym (eval expr sub_env);
                bind_pairs more
            | _ :: _ :: _ -> raise (Invalid_argument "let* keys must be symbols")
            | _::[] -> raise (Invalid_argument "let* bindings must be an even number of forms")
            | [] -> ())
            in bind_pairs bindings;
          eval body sub_env)
    | T.List { T.value = (T.Symbol "do" :: body) } ->
        List.fold_left (fun x expr -> eval expr env) T.Nil body
    | T.List { T.value = [T.Symbol "if"; test; then_expr; else_expr] } ->
        if Types.to_bool (eval test env) then (eval then_expr env) else (eval else_expr env)
    | T.List { T.value = [T.Symbol "if"; test; then_expr] } ->
        if Types.to_bool (eval test env) then (eval then_expr env) else T.Nil
    | T.List { T.value = [T.Symbol "fn*"; T.Vector { T.value = arg_names }; expr] }
    | T.List { T.value = [T.Symbol "fn*"; T.List   { T.value = arg_names }; expr] } ->
        Types.fn
          (function args ->
            let sub_env = Env.make (Some env) in
              let rec bind_args a b =
                (match a, b with
                  | [T.Symbol "&"; T.Symbol name], args -> Env.set sub_env name (Types.list args);
                  | (T.Symbol name :: names), (arg :: args) ->
                      Env.set sub_env name arg;
                      bind_args names args;
                  | [], [] -> ()
                  | _ -> raise (Invalid_argument "Bad param count in fn call"))
              in bind_args arg_names args;
              eval expr sub_env)
    | T.List { T.value = (a0 :: args) } ->
      (match eval a0 env with
         | T.Fn { T.value = f } -> f (List.map (fun x -> eval x env) args)
         | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | _ -> ast

let read str = Reader.read_str str
let print exp = Printer.pr_str exp true
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
