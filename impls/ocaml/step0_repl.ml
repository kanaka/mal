(*
  To try things at the ocaml repl:
  rlwrap ocaml

  To see type signatures of all functions:
  ocamlc -i step0_repl.ml

  To run the program:
  ocaml step0_repl.ml
*)

let eval ast = ast
let read str = str
let print exp = exp
let rep str = print (eval (read str))

let main =
  try
    while true do
      Format.printf "user> %!";
      let line = read_line () in
      Format.printf "%s\n" (rep line)
    done
  with End_of_file -> Format.printf "\n"
