fun READ s =
    readStr s

fun EVAL f =
    f

fun PRINT f =
    prStr f

fun rep s =
    s |> READ |> EVAL |> PRINT
    handle SyntaxError msg => "SYNTAX ERROR: " ^ msg

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
