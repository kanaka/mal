fun READ s =
    readStr s

fun EVAL f =
    f

fun PRINT f =
    prReadableStr f

fun rep s =
    s |> READ |> EVAL |> PRINT
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
