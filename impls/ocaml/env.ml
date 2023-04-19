module T = Types.Types
module Data = Map.Make (String)

type env = {
  outer : env option;
  data : Types.mal_type Data.t ref;
}

let make outer = { outer = outer; data = ref Data.empty }

let set env key value =
  env.data := Data.add key value !(env.data)

let rec get env key =
  match Data.find_opt key !(env.data) with
    | Some value -> Some value
    | None       -> match env.outer with
      | Some outer -> get outer key
      | None       -> None
