module Env =
  Map.Make (
    String
    (*(struct
       type t = Types.Symbol
       let compare (Types.Symbol a) (Types.Symbol b) = compare a b
    end)*)
    )

let num_fun f = Types.Fn
  (function
    | [(Types.Int a); (Types.Int b)] -> Types.Int (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let repl_env = ref (List.fold_left (fun a b -> b a) Env.empty
  [ Env.add "+" (num_fun ( + ));
    Env.add "-" (num_fun ( - ));
    Env.add "*" (num_fun ( * ));
    Env.add "/" (num_fun ( / )) ])

let rec eval_ast ast env =
  match ast with
    | Types.Symbol s ->
        (try Env.find s !env
         with Not_found -> raise (Invalid_argument ("Symbol '" ^ s ^ "' not found")))
    | Types.MalList xs -> Types.MalList (List.map (fun x -> eval x env) xs)
    | _ -> ast
and eval ast env =
  let result = eval_ast ast env in
    match result with
      | Types.MalList ((Types.Fn f) :: args) -> (f args)
      | _ -> result

let read str = Reader.read_str str
let print exp = Printer.pr_str exp
let rep str env = print (eval (read str) env)

let rec main =
  try
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
