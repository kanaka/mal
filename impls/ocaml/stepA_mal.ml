module T = Types.Types

let repl_env = Env.make (Some Core.ns)

let rec quasiquote ast =
  match ast with
    | T.List { T.value = [T.Symbol "unquote"; x] } -> x
    | T.List { T.value = xs } -> qq_list xs
    | T.Vector { T.value = xs } -> Types.list [T.Symbol "vec"; qq_list xs]
    | T.Map _ | T.Symbol _ -> Types.list [T.Symbol "quote"; ast]
    | _ -> ast
and qq_list xs = List.fold_right qq_folder xs (Types.list [])
and qq_folder elt acc =
  match elt with
    | T.List {T.value = [T.Symbol "splice-unquote"; x]} -> Types.list [T.Symbol "concat"; x; acc]
    | _ -> Types.list [T.Symbol "cons"; quasiquote elt; acc]

let rec eval env ast =
  (match Env.get env "DEBUG-EVAL" with
    | None | Some T.Nil | Some (T.Bool false) -> ()
    | Some _              ->
       Format.printf "EVAL: %a\n" (Printer.pr_str true) ast);
  match ast with
    | T.Symbol s -> (match Env.get env s with
         | Some v -> v
         | None   -> raise (Invalid_argument ("'" ^ s ^ "' not found")))
    | T.Vector { T.value = xs } -> Types.vector (List.map (eval env) xs);
    | T.Map { T.value = xs } -> Types.map (Types.MalMap.map (eval env) xs)
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
             | e -> T.String (Printexc.to_string e) in
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
let print = Printer.pr_str true
let re str = ignore (eval repl_env (read str))

let main =
    Core.init Core.ns;
    Env.set repl_env "*ARGV*"
            (Types.list (if Array.length Sys.argv > 1
                         then (List.map (fun x -> T.String x) (List.tl (List.tl (Array.to_list Sys.argv))))
                         else []));
    Env.set repl_env "eval"
            (Types.fn (function [ast] -> eval repl_env ast | _ -> T.Nil));

    re "(def! *host-language* \"ocaml\")";
    re "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))";
    re "(def! not (fn* (a) (if a false true)))";
    re "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))";

    if Array.length Sys.argv > 1 then
      re (Format.asprintf "(load-file \"%s\")" Sys.argv.(1))
    else begin
      re "(println (str \"Mal [\" *host-language* \"]\"))";
      try
        while true do
          Format.printf "user> %!";
          let line = read_line () in
          try
            Format.printf "%a\n" print (eval repl_env (read line))
          with
             | Types.MalExn exc ->
                Format.printf "mal exception: %a\n" print exc
             | e ->
                Format.printf "ocaml exception: %s\n" (Printexc.to_string e)
        done
      with End_of_file -> Format.printf "\n"
      end
