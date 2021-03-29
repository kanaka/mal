fun READ s: string =
    s

fun EVAL s: string =
    s

fun PRINT s: string =
    s

fun rep s: string =
    (PRINT o EVAL o READ) s

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
