fun def s v (ENV d) = (s, v) :: (d |> List.filter (not o eq s o #1)) |> ENV

fun lookup (ENV d) s = d |> List.find (eq s o #1) |> Option.map #2

fun wrap (ENV outer) (ENV inner) = ENV (inner @ outer)
