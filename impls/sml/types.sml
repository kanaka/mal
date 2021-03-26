datatype mal_type = NIL
                  | SYMBOL of string
                  | BOOL of bool
                  | INT of int
                  | LIST of mal_type list
                  | FN of mal_type list -> mal_type
