type mal_defs = (string * mal_type) list

datatype mal_env = ENV of mal_defs
                 | INNER of (mal_defs * mal_env)

fun get (d:mal_defs) s =
    d |> List.find (eq s o #1) |> Option.map #2

fun set s v (d:mal_defs) =
    (s, v) :: (d |> List.filter (not o eq s o #1))

fun def s v (INNER (d, out)) = INNER (set s v d, out)
  | def s v (ENV d)          = ENV (set s v d)

fun let_in s v out = INNER (set s v [], out)

fun lookup (INNER (d, out)) s = optOrElse (get d s) (fn _ => lookup out s)
  | lookup (ENV d)          s = get d s
