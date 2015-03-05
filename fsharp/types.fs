module Types

    type Node =
        | Nil
        | List of Node list
        | Vector of Node array
        | Map of Collections.Map<Node, Node>
        | Symbol of string
        | Keyword of string
        | Number of int64
        | String of string
        | Bool of bool
