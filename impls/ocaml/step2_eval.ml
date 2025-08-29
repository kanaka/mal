module T = Types.Types
module Env = Map.Make (String)

let num_fun f =
  Types.fn (function
    | [ T.Int a; T.Int b ] -> T.Int (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let repl_env =
  Env.of_list
    [
      ("+", num_fun ( + ));
      ("-", num_fun ( - ));
      ("*", num_fun ( * ));
      ("/", num_fun ( / ));
    ]

let rec eval env ast =
  (*
       Format.printf "EVAL: %a\n" (Printer.pr_str true) ast);
   *)
  match ast with
  | T.Symbol s -> (
      match Env.find_opt s env with
      | Some v -> v
      | None -> raise (Invalid_argument ("'" ^ s ^ "' not found")))
  | T.Vector { T.value = xs } -> Types.vector (List.map (eval env) xs)
  | T.Map { T.value = xs } -> Types.map (Types.MalMap.map (eval env) xs)
  | T.List { T.value = a0 :: args } -> (
      match eval env a0 with
      | T.Fn { value = f } -> f (List.map (eval env) args)
      | _ -> raise (Invalid_argument "Cannot invoke non-function"))
  | _ -> ast

let read str = Reader.read_str str
let print = Printer.pr_str true

let main =
  try
    while true do
      Format.printf "user> %!";
      let line = read_line () in
      try Format.printf "%a\n" print (eval repl_env (read line)) with
      | Types.MalExn exc -> Format.printf "mal exception: %a\n" print exc
      | e -> Format.printf "ocaml exception: %s\n" (Printexc.to_string e)
    done
  with End_of_file -> Format.printf "\n"
