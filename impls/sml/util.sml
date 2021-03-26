fun takeWhile  f xs          = takeWhile' f [] xs
and takeWhile' f acc []      = rev acc
  | takeWhile' f acc (x::xs) = if f x then takeWhile' f (x::acc) xs else rev acc

infix 3 |> fun x |> f = f x

fun eq a b = a = b
