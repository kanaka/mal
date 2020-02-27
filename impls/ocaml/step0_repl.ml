(*
  To try things at the ocaml repl:
  rlwrap ocaml

  To see type signatures of all functions:
  ocamlc -i step0_repl.ml

  To run the program:
  ocaml step0_repl.ml
*)

let read str = str
let eval ast any = ast
let print exp = exp
let rep str = print (eval (read str) "")

let rec main =
  try
    while true do
      print_string "user> ";
      print_endline (rep (read_line ()));
    done
  with End_of_file -> ()
