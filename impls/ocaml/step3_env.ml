module T = Types.Types

let num_fun f = Types.fn
  (function
    | [(T.Int a); (T.Int b)] -> T.Int (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let repl_env = Env.make None

let init_repl env = begin
  Env.set env (Types.symbol "+") (num_fun ( + ));
  Env.set env (Types.symbol "-") (num_fun ( - ));
  Env.set env (Types.symbol "*") (num_fun ( * ));
  Env.set env (Types.symbol "/") (num_fun ( / ));
end

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
