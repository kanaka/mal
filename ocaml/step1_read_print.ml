let read str = Reader.read_str str
let eval ast any = ast
let print exp = Printer.pr_str exp true
let rep str = print (eval (read str) "")

let rec main =
  try
    while true do
      print_string "user> ";
      let line = read_line () in
        try
          print_endline (rep line);
        with End_of_file -> ()
    done
  with End_of_file -> ()
