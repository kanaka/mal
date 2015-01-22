type mal_type =
  | MalList of mal_type list
  | Int of int
  | Symbol of string
  | Keyword of string
  | Nil
  | Bool of bool
  | String of string
