module Types

    type Node =
        | List of Node list
        | Vector of Node array
        | Map of Collections.Map<Node, Node>
        | Symbol of string
        | Number of int64
