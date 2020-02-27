module T = Types.Types
module Data = Map.Make (String)

type env = {
  outer : env option;
  data : Types.mal_type Data.t ref;
}

let make outer = { outer = outer; data = ref Data.empty }

let set env sym value =
  match sym with
    | T.Symbol { T.value = key } -> env.data := Data.add key value !(env.data)
    | _ -> raise (Invalid_argument "set requires a Symbol for its key")

let rec find env sym =
  match sym with
    | T.Symbol { T.value = key } ->
        (if Data.mem key !(env.data) then
           Some env
         else
           match env.outer with
             | Some outer -> find outer sym
             | None -> None)
    | _ -> raise (Invalid_argument "find requires a Symbol for its key")

let get env sym =
  match sym with
    | T.Symbol { T.value = key } ->
      (match find env sym with
         | Some found_env -> Data.find key !(found_env.data)
         | None -> raise (Invalid_argument ("'" ^ key ^ "' not found")))
    | _ -> raise (Invalid_argument "get requires a Symbol for its key")
