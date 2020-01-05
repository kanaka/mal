# {key: string, value: {kkind: kind, value: value}} -> [{kind: value.kkind, value: key}, value.value]
def _reconstruct_hash:
    map([{
        kind: .value.kkind,
        value: .key
    },
    .value.value]);

def pr_str:
    (select(.kind == "symbol")  | .value) //
    (select(.kind == "string")  | .value | tojson) //
    (select(.kind == "keyword") | ":\(.value)")  //
    (select(.kind == "number")  | .value | tostring) //
    (select(.kind == "list")    | .value | map(pr_str)  | join(" ") | "(\(.))") //
    (select(.kind == "vector")  | .value | map(pr_str)  | join(" ") | "[\(.)]") //
    (select(.kind == "hashmap") | .value | to_entries | _reconstruct_hash | add // [] | map(pr_str) | join(" ") | "{\(.)}") //
    (select(.kind == "nil")     | "nil") //
    (select(.kind == "true")    | "true") //
    (select(.kind == "false")   | "false") //
    "#<Unknown \(.kind) in \(.)>";