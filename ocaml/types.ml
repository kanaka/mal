type mal_type =
  | List of mal_type list
  | Vector of mal_type list
  | Map of mal_type list
  | Int of int
  | Symbol of string
  | Keyword of string
  | Nil
  | Bool of bool
  | String of string
  | Fn of (mal_type list -> mal_type)

let to_bool x = match x with
  | Nil | Bool false -> false
  | _ -> true
