module Types
    
    type Node =
        | List of list<Node>
        | Symbol of string
        | Number of int64
