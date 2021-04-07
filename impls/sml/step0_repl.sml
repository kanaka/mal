fun read s: string =
    s

fun eval s: string =
    s

fun print s: string =
    s

fun rep s: string =
    (print o eval o read) s

fun repl () =
    let open TextIO
    in (
        print("user> ");
        case inputLine(stdIn) of
            SOME(line) => (
                print(rep(line) ^ "\n");
                repl ()
            )
            | NONE => ()
    ) end

fun main () = repl ()
