fun set s v (NS d) = d := (s, v) :: (!d |> List.filter (not o eq s o #1))

fun get (NS d) s = !d |> List.find (eq s o #1) |> Option.map #2

fun def s v (ENV ns)        = set s v ns
  | def s v (INNER (ns, _)) = set s v ns

fun lookup (ENV ns)            s = get ns s
  | lookup (INNER (ns, outer)) s = optOrElse (get ns s) (fn () => lookup outer s)

fun inside outer = INNER (NS (ref []), outer)
