module Types

    type Node =
        | List of Node list
        | Vector of Node array
        | Symbol of string
        | Number of int64
