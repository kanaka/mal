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
