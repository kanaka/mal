# {key: string, value: {kkind: kind, value: value}} -> [{kind: value.kkind, value: key}, value.value]
def _reconstruct_hash:
    map([{
        kind: .value.kkind,
        value: .key
    },
    .value.value]);

def pr_str(opt):
    (select(.kind == "symbol")  | .value) //
    (select(.kind == "string")  | .value | if opt.readable then tojson|tojson else tojson end) //
    (select(.kind == "keyword") | ":\(.value)")  //
    (select(.kind == "number")  | .value | tostring) //
    (select(.kind == "list")    | .value | map(pr_str(opt))  | join(" ") | "(\(.))") //
    (select(.kind == "vector")  | .value | map(pr_str(opt))  | join(" ") | "[\(.)]") //
    (select(.kind == "hashmap") | .value | to_entries | _reconstruct_hash | add // [] | map(pr_str(opt)) | join(" ") | "{\(.)}") //
    (select(.kind == "nil")     | "nil") //
    (select(.kind == "true")    | "true") //
    (select(.kind == "false")   | "false") //
    (select(.kind == "fn")      | "#<fn \(.function)>") //
    (select(.kind == "function")| "#<function \([":anon"] + .names | join(", "))>") //
    (select(.kind == "atom")    | "(atom \(.value | pr_str(opt)))") //
    "#<Unknown \(.kind) in \(.)>";

def pr_str:
    pr_str({readable: true});