type mal_defs = (string * mal_type) list

datatype mal_env = ENV of mal_defs
                 | INNER of (mal_defs * mal_env)

fun get (d:mal_defs) s =
    d |> List.find (eq s o #1) |> Option.map #2

fun set s v (d:mal_defs) =
    (s, v) :: (d |> List.filter (not o eq s o #1))

fun lookup (INNER (d, out)) s = (case get d s of NONE => lookup out s | x => x)
  | lookup (ENV d)          s = get d s
