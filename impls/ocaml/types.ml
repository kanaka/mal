module rec Types
  : sig
  type 'a with_meta = { value : 'a; meta : t }
  and t =
    | List of t list with_meta
    | Vector of t list with_meta
    | Map of t MalMap.t with_meta
    | Int of int
    | Symbol of string with_meta
    | Keyword of string
    | Nil
    | Bool of bool
    | String of string
    | Fn of (t list -> t) with_meta
    | Atom of t ref
  end = Types

and MalValue
  : sig
    type t = Types.t
    val compare : t -> t -> int
  end
  = struct
    type t = Types.t
    let compare = Pervasives.compare
  end

and MalMap
  : Map.S with type key = MalValue.t
  = Map.Make(MalValue)

exception MalExn of Types.t

let to_bool x = match x with
  | Types.Nil | Types.Bool false -> false
  | _ -> true

type mal_type = MalValue.t

let list   x = Types.List   { Types.value = x; meta = Types.Nil }
let map    x = Types.Map    { Types.value = x; meta = Types.Nil }
let vector x = Types.Vector { Types.value = x; meta = Types.Nil }
let symbol x = Types.Symbol { Types.value = x; meta = Types.Nil }
let fn     f = Types.Fn     { Types.value = f; meta = Types.Nil }

let rec list_into_map target source =
  match source with
    | k :: v :: more -> list_into_map (MalMap.add k v target) more
    | [] -> map target
    | _ :: [] -> raise (Invalid_argument "Literal maps must contain an even number of forms")

let rec mal_list_equal a b =
  List.length a = List.length b && List.for_all2 mal_equal a b

and mal_hash_equal a b =
  if MalMap.cardinal a = MalMap.cardinal b
  then
    let identical_to_b k v = MalMap.mem k b && mal_equal v (MalMap.find k b) in
    MalMap.for_all identical_to_b a
  else false

and mal_equal a b =
  match (a, b) with
    | (Types.List a, Types.List b)
    | (Types.List a, Types.Vector b)
    | (Types.Vector a, Types.List b)
    | (Types.Vector a, Types.Vector b) -> mal_list_equal a.Types.value b.Types.value
    | (Types.Map a, Types.Map b) -> mal_hash_equal a.Types.value b.Types.value
    | _ -> a = b
