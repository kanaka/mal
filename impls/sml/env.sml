type mal_defs = (string * mal_type) list

datatype mal_env = ENV of mal_defs

fun get (d:mal_defs) s =
    d |> List.find (eq s o #1) |> Option.map #2

fun set s v (d:mal_defs) =
    (s, v) :: (d |> List.filter (not o eq s o #1))

fun def s v (ENV d) = ENV (set s v d)

fun lookup (ENV d) s = get d s
