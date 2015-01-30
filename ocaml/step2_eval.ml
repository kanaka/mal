module T = Types.Types

module Env =
  Map.Make (
    String
    (*(struct
       type t = Types.Symbol
       let compare (Types.Symbol a) (Types.Symbol b) = compare a b
    end)*)
    )

let num_fun f = Types.fn
  (function
    | [(T.Int a); (T.Int b)] -> T.Int (f a b)
    | _ -> raise (Invalid_argument "Numeric args required for this Mal builtin"))

let repl_env = ref (List.fold_left (fun a b -> b a) Env.empty
  [ Env.add "+" (num_fun ( + ));
    Env.add "-" (num_fun ( - ));
    Env.add "*" (num_fun ( * ));
    Env.add "/" (num_fun ( / )) ])

let rec eval_ast ast env =
  match ast with
    | T.Symbol { T.value = s } ->
       (try Env.find s !env
        with Not_found -> raise (Invalid_argument ("Symbol '" ^ s ^ "' not found")))
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
  let result = eval_ast ast env in
    match result with
      | T.List { T.value = ((T.Fn { T.value = f }) :: args) } -> (f args)
      | _ -> result

let read str = Reader.read_str str
let print exp = Printer.pr_str exp true
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
