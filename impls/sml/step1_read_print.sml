fun read s =
    readStr s

fun eval f =
    f

fun print f =
    prReadableStr f

fun rep s =
    s |> read |> eval |> print
    handle SyntaxError msg => "SYNTAX ERROR: " ^ msg
         | Nothing         => ""

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
