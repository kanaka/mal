fun takeWhile  f xs          = takeWhile' f [] xs
and takeWhile' f acc []      = rev acc
  | takeWhile' f acc (x::xs) = if f x then takeWhile' f (x::acc) xs else rev acc

infix 3 |> fun x |> f = f x

fun eq a b = a = b

fun optOrElse NONE b = b ()
  | optOrElse a    _ = a

fun valOrElse (SOME x) _ = x
  | valOrElse a        b = b ()

fun optIfNone b NONE = b ()
  | optIfNone _ a    = a

fun valIfNone _ (SOME a) = a
  | valIfNone b _        = b ()

fun interleave (x::xs) (y::ys) = x :: y :: interleave xs ys
  | interleave []      ys      = ys
  | interleave xs      []      = xs

fun identity x = x

fun triml k s = String.extract (s, k, NONE)

fun trimr k s = String.substring (s, 0, String.size s - k)

fun malEscape s = String.translate (fn #"\"" => "\\\""
                                     | #"\n" => "\\n"
                                     | #"\\" => "\\\\"
                                     | c => String.str c) s

fun malUnescape s = malUnescape' (String.explode s)
and malUnescape' (#"\\"::(#"\""::rest)) = "\"" ^ malUnescape' rest
  | malUnescape' (#"\\"::(#"n" ::rest)) = "\n" ^ malUnescape' rest
  | malUnescape' (#"\\"::(#"\\"::rest)) = "\\" ^ malUnescape' rest
  | malUnescape' (c::rest)              = (String.str c) ^ malUnescape' rest
  | malUnescape' ([])                   = ""

