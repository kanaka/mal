module T = Types.Types

let num_fun f =
  Types.fn (function
    | [ T.Int a; T.Int b ] -> T.Int (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let repl_env = Env.make None

let init_repl env =
  Env.set env "+" (num_fun ( + ));
  Env.set env "-" (num_fun ( - ));
  Env.set env "*" (num_fun ( * ));
  Env.set env "/" (num_fun ( / ))

let rec eval env ast =
  (match Env.get env "DEBUG-EVAL" with
  | None | Some T.Nil | Some (T.Bool false) -> ()
  | Some _ -> Format.printf "EVAL: %a\n" (Printer.pr_str true) ast);
  match ast with
  | T.Symbol s -> (
      match Env.get env s with
      | Some v -> v
      | None -> raise (Invalid_argument ("'" ^ s ^ "' not found")))
  | T.Vector { T.value = xs } -> Types.vector (List.map (eval env) xs)
  | T.Map { T.value = xs } -> Types.map (Types.MalMap.map (eval env) xs)
  | T.List { T.value = [ T.Symbol "def!"; T.Symbol key; expr ] } ->
      let value = eval env expr in
      Env.set env key value;
      value
  | T.List
      { T.value = [ T.Symbol "let*"; T.Vector { T.value = bindings }; body ] }
  | T.List
      { T.value = [ T.Symbol "let*"; T.List { T.value = bindings }; body ] } ->
      let sub_env = Env.make (Some env) in
      let rec bind_pairs = function
        | T.Symbol sym :: expr :: more ->
            Env.set sub_env sym (eval sub_env expr);
            bind_pairs more
        | _ :: _ :: _ -> raise (Invalid_argument "let* keys must be symbols")
        | _ :: [] ->
            raise
              (Invalid_argument "let* bindings must be an even number of forms")
        | [] -> ()
      in
      bind_pairs bindings;
      eval sub_env body
  | T.List { T.value = a0 :: args } -> (
      match eval env a0 with
      | T.Fn { value = f } -> f (List.map (eval env) args)
      | _ -> raise (Invalid_argument "Cannot invoke non-function"))
  | _ -> ast

let read str = Reader.read_str str
let print = Printer.pr_str true

let main =
  init_repl repl_env;

  try
    while true do
      Format.printf "user> %!";
      let line = read_line () in
      try Format.printf "%a\n" print (eval repl_env (read line)) with
      | Types.MalExn exc -> Format.printf "mal exception: %a\n" print exc
      | e -> Format.printf "ocaml exception: %s\n" (Printexc.to_string e)
    done
  with End_of_file -> Format.printf "\n"
