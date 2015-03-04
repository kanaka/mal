require! {
    LiveScript
    fs
    'prelude-ls': {join, sum, all, fold, fold1, map}
    './types.ls': {string, Builtin, Lambda, MalList, MalVec, MalMap}
    './builtins.ls': {NIL, is-number, is-nil, is-seq, mal-eql}
    './printer.ls': {str, pr-str}
    './reader.ls': {read-str}
}

export ns = {}

bool = (value) -> type: \BOOL, value: value
int = (value) -> type: \INT, value: value
float = (value) -> type: \FLOAT, value: value

ns.list = new Builtin (elems) -> new MalList elems

ns['list?'] = new Builtin ([x]) -> bool x.type is \LIST

ns['typeof?'] = new Builtin ([x]) -> string x.type

ns['empty'] = new Builtin ([x]) -> switch x.type
    | \LIST => new MalList []
    | \VEC => new MalVec []
    | \MAP => new MalMap []
    | \STRING => string ""
    | _ => throw new Error("Bad argument to empty: #{ pr-str x }")

ns['empty?'] = new Builtin ([x]) ->
    | is-nil x => bool true
    | (is-seq x) or (x.type is \STRING) => bool x.value.length is 0
    | _ => throw new Error 'Not a sequence'

ns['range'] = new Builtin ([min, max]:args) ->
    if args.length is 1
        [min, max] = [{value: 0}, min]
    new MalList [min.value to max.value].map int

ns['nil?'] = new Builtin ([x]) -> bool is-nil x

ns['count'] = new Builtin ([x]) ->
    | is-nil x => int 0
    | is-seq x => int x.value.length
    | _ => throw new Error 'Not a sequence'

ns['='] = new Builtin ([x, ...xs]:args) ->
    throw new Error 'Expected at least two arguments' unless args.length > 1
    bool all (-> mal-eql x, it), xs

ns['cons'] = new Builtin ([x, xs]:args) ->
    throw new Error 'Two arguments expected' unless args.length is 2
    throw new Error 'Collection must be sequence' unless is-seq xs
    elems = [x] ++ xs.value
    switch xs.type
        | \VEC => new MalVec elems
        | \LIST => new MalList elems

ns['concat'] = new Builtin (xss) ->
    throw new Error("Arguments must be sequences") unless all is-seq, xss
    new MalList fold (++), [], map (.value), xss

ns['name'] = new Builtin ([x]:args) ->
    throw new Error 'One argument expected' unless args.length is 1
    throw new Error 'Not a keyword' unless x.type is \KEYWORD
    {type: \STRING, value: x.name}

ns['get'] = new Builtin ([m, k]:args) ->
    throw new Error 'Two arguments expected' unless args.length is 2
    throw new Error 'Not a map' unless m.type is \MAP
    (m.get k) or NIL

ns['has-key?'] = new Builtin ([m, k]:args) ->
    throw new Error 'Two arguments expected' unless args.length is 2
    throw new Error 'Not a map' unless m.type is \MAP
    bool (m.get k)?

assoc-num-op = (op, zero, toMal) -> new Builtin (nums) ->
    throw new Error 'Expected at least two numbers' unless nums.length > 1
    throw new Error "Expected numbers" unless all is-number nums
    toMal fold op, zero, (map (.value),  nums)

int-op = (op, zero) -> assoc-num-op op, zero, int

comparator = (test) -> new Builtin ([x, ...xs]:nums) ->
    throw new Error 'Expected at least two numbers' unless nums.length > 1
    throw new Error "Expected numbers" unless all is-number nums
    init = {val: x, ret: true}
    f = ({val: a, ret}, b) -> val: b, ret: (ret and (test a.value, b.value))
    bool (.ret) fold f, init, xs

ns['<'] = comparator (<)
ns['<='] = comparator (<=)
ns['>'] = comparator (>)
ns['>='] = comparator (>=)
ns['+'] = int-op (+), 0
ns['*'] = int-op (*), 1
ns['/'] = new Builtin ([x, y]) ->
    if x.type is \INT and y.type is \INT
        int Math.round x.value / y.value
    else
        float x.value / y.value

ns['%'] = new Builtin ([x, y]) ->
    if x.type is \INT and y.type is \INT
        int x.value % y.value
    else
        float x.value % y.value

ns['-'] = new Builtin ([x, ...ys]:args) ->
    val = x.value - sum map (.value), ys
    if all (-> it.type is \INT), args
        int val
    else
        float val

ns['print'] = new Builtin (exprs) ->
    for e in exprs
        process.stdout.write str e
    NIL

ns['println'] = new Builtin (exprs) ->
    console.log join '',  (map str, exprs)
    NIL

ns['pr-str'] = new Builtin string . (join ' ') . (map pr-str)

ns['str'] = new Builtin string . (join '') . (map str)

ns['prn'] = new Builtin (exprs) ->
    console.log join ' ',  (map pr-str, exprs)
    NIL

ns['read-string'] = new Builtin ([str]:args) ->
    throw new Error("Too many arguments to read-string") if args.length > 1
    throw new Error("No string to read") unless str?
    throw new Error("Argument must be a string") unless str.type is \STRING
    read-str str.value

ns['slurp'] = new Builtin ([filename, encoding]:args) ->
    throw new Error("Too many arguments to slurp") if args.length > 2
    throw new Error("No filename to read") unless str?
    throw new Error("Filename must be a string") unless filename.type is \STRING
    throw new Error("Encoding must be a string") if (encoding and encoding.type isnt \STRING)
    encoding ?= {value: 'utf8'}
    string fs.readFileSync filename.value, encoding.value
