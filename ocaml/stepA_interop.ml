module T = Types.Types

let repl_env = Env.make (Some Core.ns)

let rec quasiquote ast =
  match ast with
    | T.List   { T.value = [T.Symbol {T.value = "unquote"}; ast] } -> ast
    | T.Vector { T.value = [T.Symbol {T.value = "unquote"}; ast] } -> ast
    | T.List   { T.value = T.List { T.value = [T.Symbol {T.value = "splice-unquote"}; head]} :: tail }
    | T.Vector { T.value = T.List { T.value = [T.Symbol {T.value = "splice-unquote"}; head]} :: tail } ->
       Types.list [Types.symbol "concat"; head; quasiquote (Types.list tail)]
    | T.List   { T.value = head :: tail }
    | T.Vector { T.value = head :: tail } ->
       Types.list [Types.symbol "cons"; quasiquote head; quasiquote (Types.list tail) ]
    | ast -> Types.list [Types.symbol "quote"; ast]

let kw_macro = T.Keyword "macro"

let is_macro_call ast env =
  match ast with
  | T.List { T.value = s :: args } ->
     (match (try Env.get env s with _ -> T.Nil) with
      | T.Fn { T.meta = T.Map { T.value = meta } }
        -> Types.MalMap.mem kw_macro meta && Types.to_bool (Types.MalMap.find kw_macro meta)
      | _ -> false)
  | _ -> false

let rec macroexpand ast env =
  if is_macro_call ast env
  then match ast with
       | T.List { T.value = s :: args } ->
          (match (try Env.get env s with _ -> T.Nil) with
           | T.Fn { T.value = f } -> macroexpand (f args) env
           | _ -> ast)
       | _ -> ast
  else ast

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
  match macroexpand ast env with
    | T.List { T.value = [(T.Symbol { T.value = "def!" }); key; expr] } ->
        let value = (eval expr env) in
          Env.set env key value; value
    | T.List { T.value = [(T.Symbol { T.value = "defmacro!" }); key; expr] } ->
       (match (eval expr env) with
          | T.Fn { T.value = f; T.meta = meta } ->
             let fn = T.Fn { T.value = f; meta = Core.assoc [meta; kw_macro; (T.Bool true)]}
             in Env.set env key fn; fn
          | _ -> raise (Invalid_argument "devmacro! value must be a fn"))
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
    | T.List { T.value = [T.Symbol { T.value = "quote" }; ast] } -> ast
    | T.List { T.value = [T.Symbol { T.value = "quasiquote" }; ast] } ->
       eval (quasiquote ast) env
    | T.List { T.value = [T.Symbol { T.value = "macroexpand" }; ast] } ->
       macroexpand ast env
    | T.List { T.value = [T.Symbol { T.value = "try*" }; scary ;
                          T.List { T.value = [T.Symbol { T.value = "catch*" };
                                              local ; handler]}]} ->
       (try (eval scary env)
        with exn ->
           let value = match exn with
             | Types.MalExn value -> value
             | Invalid_argument msg -> T.String msg
             | _ -> (T.String "OCaml exception") in
           let sub_env = Env.make (Some env) in
           Env.set sub_env local value;
           eval handler sub_env)
    | T.List _ as ast ->
      (match eval_ast ast env with
         | T.List { T.value = ((T.Fn { T.value = f }) :: args) } -> f args
         | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | ast -> eval_ast ast env

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

    ignore (rep "(def! *host-language* \"ocaml\")" repl_env);
    ignore (rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" repl_env);
    ignore (rep "(def! not (fn* (a) (if a false true)))" repl_env);
    ignore (rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" repl_env);
    ignore (rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))" repl_env);

    if Array.length Sys.argv > 1 then
      ignore (rep ("(load-file \"" ^ Sys.argv.(1) ^ "\")") repl_env)
    else begin
        ignore (rep "(println (str \"Mal [\" *host-language* \"]\"))" repl_env);
        while true do
          print_string "user> ";
          let line = read_line () in
          try
            print_endline (rep line repl_env);
          with End_of_file -> ()
             | Invalid_argument x ->
                output_string stderr ("Invalid_argument exception: " ^ x ^ "\n");
                flush stderr
             | _ ->
                output_string stderr ("Erroringness!\n");
                flush stderr
        done
      end
  with End_of_file -> ()
