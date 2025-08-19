module T = Types.Types

let repl_env = Env.make (Some Core.ns)

let rec quasiquote ast =
  match ast with
    | T.List   { T.value = [T.Symbol "unquote"; ast] } -> ast
    | T.List {T.value = xs} -> List.fold_right qq_folder xs (Types.list [])
    | T.Vector {T.value = xs} -> Types.list [T.Symbol "vec";
                                             List.fold_right qq_folder xs (Types.list [])]
    | T.Map    _ -> Types.list [T.Symbol "quote"; ast]
    | T.Symbol _ -> Types.list [T.Symbol "quote"; ast]
    | _ -> ast
and qq_folder elt acc =
  match elt with
    | T.List {T.value = [T.Symbol "splice-unquote"; x]} -> Types.list [T.Symbol "concat"; x; acc]
    | _ -> Types.list [T.Symbol "cons"; quasiquote elt; acc]

let rec eval env ast =
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
      -> T.Vector { T.value = List.map (eval env) xs;
                    T.meta = meta }
    | T.Map { T.value = xs; T.meta = meta }
      -> T.Map {T.meta = meta;
                T.value = (Types.MalMap.fold
                             (fun k v m
                              -> Types.MalMap.add k (eval env v) m)
                             xs
                             Types.MalMap.empty)}
    | T.List { T.value = [T.Symbol "def!"; T.Symbol key; expr] } ->
        let value = eval env expr in
          Env.set env key value; value
    | T.List { T.value = [T.Symbol "defmacro!"; T.Symbol key; expr] } ->
       (match eval env expr with
          | T.Fn ({ macro = false } as f) ->
             let fn = T.Fn { f with macro = true }
             in Env.set env key fn; fn
          | _ -> raise (Invalid_argument "defmacro! value must be a fn"))
    | T.List { T.value = [T.Symbol "let*"; (T.Vector { T.value = bindings }); body] }
    | T.List { T.value = [T.Symbol "let*"; (T.List   { T.value = bindings }); body] } ->
        (let sub_env = Env.make (Some env) in
          let rec bind_pairs = (function
            | T.Symbol sym :: expr :: more ->
                Env.set sub_env sym (eval sub_env expr);
                bind_pairs more
            | _ :: _ :: _ -> raise (Invalid_argument "let* keys must be symbols")
            | _::[] -> raise (Invalid_argument "let* bindings must be an even number of forms")
            | [] -> ())
            in bind_pairs bindings;
          eval sub_env body)
    | T.List { T.value = (T.Symbol "do" :: body) } ->
        List.fold_left (fun _ -> eval env) T.Nil body
    | T.List { T.value = [T.Symbol "if"; test; then_expr; else_expr] } ->
       eval env (match eval env test with | T.Nil | T.Bool false -> else_expr
                                          | _ -> then_expr)
    | T.List { T.value = [T.Symbol "if"; test; then_expr] } ->
       (match eval env test  with | T.Nil | T.Bool false -> T.Nil
                                  | _ -> eval env then_expr)
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
              eval sub_env expr)
    | T.List { T.value = [T.Symbol "quote"; ast] } -> ast
    | T.List { T.value = [T.Symbol "quasiquote"; ast] } ->
       eval env (quasiquote ast)
    | T.List { T.value = [T.Symbol "try*"; scary]} ->
       eval env scary
    | T.List { T.value = [T.Symbol "try*"; scary ;
                          T.List { T.value = [T.Symbol "catch*";
                                              T.Symbol local ; handler]}]} ->
       (try eval env scary
        with exn ->
           let value = match exn with
             | Types.MalExn value -> value
             | Invalid_argument msg -> T.String msg
             | e -> (T.String (Printexc.to_string e)) in
           let sub_env = Env.make (Some env) in
           Env.set sub_env local value;
           eval sub_env handler)
    | T.List { T.value = (a0 :: args) } ->
      (match eval env a0 with
         | T.Fn { value = f; macro = true } -> eval env (f args)
         | T.Fn { value = f } -> f (List.map (eval env) args)
         | _ -> raise (Invalid_argument "Cannot invoke non-function"))
    | _ -> ast

let read str = Reader.read_str str
let print exp = Printer.pr_str exp true
let rep str env = print (eval env (read str))

let rec main =
  try
    Core.init Core.ns;
    Env.set repl_env "*ARGV*"
            (Types.list (if Array.length Sys.argv > 1
                         then (List.map (fun x -> T.String x) (List.tl (List.tl (Array.to_list Sys.argv))))
                         else []));
    Env.set repl_env "eval"
            (Types.fn (function [ast] -> eval repl_env ast | _ -> T.Nil));

    ignore (rep "(def! *host-language* \"ocaml\")" repl_env);
    ignore (rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))" repl_env);
    ignore (rep "(def! not (fn* (a) (if a false true)))" repl_env);
    ignore (rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" repl_env);

    if Array.length Sys.argv > 1 then
      try
        ignore (rep ("(load-file \"" ^ Sys.argv.(1) ^ "\")") repl_env);
      with
        | Types.MalExn exc ->
           output_string stderr ("Exception: " ^ (print exc) ^ "\n");
           flush stderr
    else begin
        ignore (rep "(println (str \"Mal [\" *host-language* \"]\"))" repl_env);
        while true do
          print_string "user> ";
          let line = read_line () in
          try
            print_endline (rep line repl_env);
          with End_of_file -> ()
             | Types.MalExn exc ->
                output_string stderr ("Exception: " ^ (print exc) ^ "\n");
                flush stderr
             | Invalid_argument x ->
                output_string stderr ("Invalid_argument exception: " ^ x ^ "\n");
                flush stderr
             | e ->
                output_string stderr ((Printexc.to_string e) ^ "\n");
                flush stderr
        done
      end
  with End_of_file -> ()
