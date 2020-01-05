def pr_str:
    (select(.kind == "symbol")  | .value) //
    (select(.kind == "string")  | .value | tojson) //
    (select(.kind == "keyword") | ":\(.value)")  //
    (select(.kind == "number")  | .value | tostring) //
    (select(.kind == "list")    | .values | map(pr_str) | join(" ") | "(\(.))") //
    (select(.kind == "nil")     | "nil") //
    (select(.kind == "true")    | "true") //
    (select(.kind == "false")   | "false") //
    "#<Unknown \(.kind) in \(.)>";