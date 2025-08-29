let eval ast = ast
let read str = Reader.read_str str
let print = Printer.pr_str true

let main =
  try
    while true do
      Format.printf "user> %!";
      let line = read_line () in
      try Format.printf "%a\n" print (eval (read line)) with
      | Types.MalExn exc -> Format.printf "mal exception: %a\n" print exc
      | e -> Format.printf "ocaml exception: %s\n" (Printexc.to_string e)
    done
  with End_of_file -> Format.printf "\n"
