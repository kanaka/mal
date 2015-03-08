require! {
    LiveScript
    fs
    'prelude-ls': {join, sum, all, fold, fold1, map}
    './types.ls': {sym, int, keyword, string, Builtin, Lambda, MalList, MalVec, MalMap}
    './builtins.ls': {NIL, is-number, is-nil, is-seq, mal-eql, to-pairs}
    './printer.ls': {str, pr-str}
    './reader.ls': {read-str}
}

export ns = {}

bool = (value) -> type: \BOOL, value: value
float = (value) -> type: \FLOAT, value: value

ns.list = new Builtin (elems) -> new MalList elems

ns['list?'] = new Builtin ([x]) -> bool x.type is \LIST

ns['seq?'] = new Builtin bool . is-seq . (.0)

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

ns['nth'] = new Builtin ([xs, n]) ->
    throw new Error("Index must be an integer, got #{ n.type }") unless n.type is \INT
    throw new Error("Collection must be indexable, got #{ xs.type }") unless is-seq xs
    throw new Error("Index out of range") unless 0 <= n.value < xs.value.length
    xs.value[n.value]

ns['range'] = new Builtin ([min, max]:args) ->
    if args.length > 2 or not args.length
        throw new Error("Wrong number of arguments to range, got #{ args.length }")
    if args.length is 1
        [min, max] = [{value: 0}, min]
    new MalList [min.value to max.value].map int

ns['nil?'] = new Builtin ([x]) -> bool is-nil x

ns['symbol'] = new Builtin ([x]:args) ->
    throw new Error("Expected one argument to symbol, got #{ args.length }") unless args.length is 1
    throw new Error("Argument must be a string") unless x.type is \STRING
    sym x.value

ns['symbol?'] = new Builtin ([x]:args) ->
    throw new Error("Expected one argument to symbol?, got #{ args.length }") unless args.length is 1
    bool x.type is \SYM

ns['keyword'] = new Builtin ([x]:args) ->
    throw new Error("Expected one argument to keyword, got #{ args.length }") unless args.length is 1
    throw new Error("Argument must be a string") unless x.type is \STRING
    keyword x.value

ns['keyword?'] = new Builtin ([x]:args) ->
    throw new Error("Expected one argument to keyword?, got #{ args.length }") unless args.length is 1
    bool x.type is \KEYWORD

ns['vector'] = new Builtin ([x]:args) ->
    throw new Error("Expected one argument to vector, got #{ args.length }") unless args.length is 1
    throw new Error("Argument must be a sequence") unless is-seq x
    new MalVec x.value

ns['vector?'] = new Builtin ([x]:args) ->
    throw new Error("Expected one argument to vector?, got #{ args.length }") unless args.length is 1
    bool x.type is \VEC

ns['count'] = new Builtin ([x]) ->
    | is-nil x => int 0
    | is-seq x => int x.value.length
    | _ => throw new Error 'Not a sequence'

ns['='] = new Builtin ([x, ...xs]:args) ->
    throw new Error 'Expected at least two arguments' unless args.length > 1
    bool all (-> mal-eql x, it), xs

# Add an element to the beginning of a sequence.
ns['cons'] = new Builtin ([x, xs]:args) ->
    throw new Error "Two arguments expected to cons, got: #{ args.length }" unless args.length is 2
    throw new Error 'Collection must be sequence' unless is-seq xs
    xs.cons x

# Add an element at the end of a sequence.
ns['conj'] = new Builtin ([xs, ...additions]) ->
    throw new Error 'Collection must be sequence' unless is-seq xs
    fold ((coll, e) -> coll.conj e), xs, additions

ns['concat'] = new Builtin (xss) ->
    throw new Error("Arguments must be sequences") unless all is-seq, xss
    new MalList fold (++), [], map (.value), xss

ns['last'] = new Builtin ([xs]) ->
    xs.value[xs.value.length - 1] or NIL

ns['initial'] = new Builtin ([xs]) ->
    new MalList xs.value.slice(0, xs.value.length - 1)

ns['name'] = new Builtin ([x]:args) ->
    throw new Error 'One argument expected' unless args.length is 1
    throw new Error 'Not a keyword' unless x.type is \KEYWORD
    {type: \STRING, value: x.name}

ns['get'] = new Builtin ([m, k]:args) ->
    throw new Error 'Two arguments expected' unless args.length is 2
    switch m.type
        | \NIL => NIL
        | \MAP => (m.get k) or NIL
        | _ => throw new Error "Cannot get from #{ pr-str m }"

ns['has-key?'] = new Builtin ([m, k]:args) ->
    throw new Error 'Two arguments expected' unless args.length is 2
    throw new Error 'Not a map' unless m.type is \MAP
    bool (m.get k)?

ns['keys'] = new Builtin ([m]) ->
    new MalList m.keys()

ns['vals'] = new Builtin ([m]) ->
    new MalList [m.get k for k in m.keys()]

ns['contains?'] = new Builtin ([coll, e]) ->
    throw new Error("Not a collection") unless coll.type in <[ VEC LIST MAP ]>
    bool coll.contains e

ns['assoc'] = new Builtin ([m, k, v]) ->
    m.assoc k, v

ns['dissoc'] = new Builtin ([m, ...ks]) ->
    m.dissoc ks

ns['hash-map'] = new Builtin ([m, ...vals]) ->
    new MalMap to-pairs vals

ns['map?'] = new Builtin ([m]) ->
    bool m.type is \MAP

ns['vector'] = new Builtin (args) ->
    new MalVec args

ns['sequential?'] = new Builtin ([x]) ->
    bool is-seq x

assoc-num-op = (op, zero, toMal) -> new Builtin (nums) ->
    throw new Error 'Expected at least two numbers' unless nums.length > 1
    throw new Error "Expected numbers" unless all is-number, nums
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
    console.log join ' ', (map str, exprs)
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

ns['throw'] = new Builtin ([msg]) -> throw new Error(msg)

ns['deref'] = new Builtin ([atom]) ->
    unless atom.type is \ATOM
        throw new Error("Expected a reference, got #{ atom.type }")
    atom.value

ns['reset!'] = new Builtin ([atom, new-value]) ->
    unless atom.type is \ATOM
        throw new Error("Expected a reference, got #{ atom.type }")
    atom.value = (new-value or NIL)
