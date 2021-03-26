datatype mal_env = ENV of (string * mal_type) list

fun lookup (ENV fs) s =
    fs |> List.find (eq s o #1)
       |> Option.map #2
