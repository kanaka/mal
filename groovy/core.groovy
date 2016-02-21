import types
import types.MalException
import types.MalSymbol
import reader
import printer

class core {
    def static do_pr_str(args) {
        return printer._pr_list(args, " ", true)
    }
    def static do_str(args) {
        return printer._pr_list(args, "", false)
    }
    def static do_prn(args) {
        println(printer._pr_list(args, " ", true))
    }
    def static do_println(args) {
        println(printer._pr_list(args, " ", false))
    }

    def static do_concat(args) {
        args.inject([], { a, b -> a + (b as List) })
    }
    def static do_nth(args) {
        if (args[0].size() <= args[1]) {
            throw new MalException("nth: index out of range")
        }
        args[0][args[1]]
    }
    def static do_conj(args) {
        if (types.list_Q(args[0])) {
            args.drop(1).inject(args[0], { a, b -> [b] + a })
        } else {
            types.vector(args.drop(1).inject(args[0], { a, b -> a + [b] }))
        }
    }
    def static do_apply(args) {
        def start_args = args.drop(1).take(args.size()-2) as List
        args[0](start_args + (args.last() as List))
    }

    def static do_swap_BANG(args) {
        def (atm,f) = [args[0], args[1]]
        atm.value = f([atm.value] + (args.drop(2) as List))
    }

    static ns = [
        "=": { a -> a[0]==a[1]},
        "throw": { a -> throw new MalException(a[0]) },

        "nil?": { a -> a[0] == null },
        "true?": { a -> a[0] == true },
        "false?": { a -> a[0] == false },
        "symbol": { a -> new MalSymbol(a[0]) },
        "symbol?": { a -> a[0] instanceof MalSymbol },
        "keyword": { a -> types.keyword(a[0]) },
        "keyword?": { a -> types.keyword_Q(a[0]) },

        "pr-str": core.&do_pr_str,
        "str": core.&do_str,
        "prn": core.&do_prn,
        "println": core.&do_println,
        "read-string": reader.&read_str,
        "readline": { a -> System.console().readLine(a[0]) },
        "slurp": { a -> new File(a[0]).text },

        "<":  { a -> a[0]<a[1]},
        "<=": { a -> a[0]<=a[1]},
        ">":  { a -> a[0]>a[1]},
        ">=": { a -> a[0]>=a[1]},
        "+":  { a -> a[0]+a[1]},
        "-":  { a -> a[0]-a[1]},
        "*":  { a -> a[0]*a[1]},
        "/":  { a -> a[0]/a[1]},  // /
        "time-ms": { a -> System.currentTimeMillis() },

        "list": { a -> a},
        "list?": { a -> types.list_Q(a[0]) },
        "vector": { a -> types.vector(a) },
        "vector?": { a -> types.vector_Q(a[0]) },
        "hash-map": { a -> types.hash_map(a) },
        "map?": { a -> types.hash_map_Q(a[0]) },
        "assoc": { a -> types.assoc_BANG(types.copy(a[0]), a.drop(1)) },
        "dissoc": { a -> types.dissoc_BANG(types.copy(a[0]), a.drop(1)) },
        "get": { a ->  a[0] == null ? null : a[0][a[1]] },
        "contains?": { a -> a[0].containsKey(a[1]) },
        "keys": { a -> a[0].keySet() as List },
        "vals": { a -> a[0].values() as List },

        "sequential?": { a -> types.&sequential_Q(a[0]) },
        "cons": { a -> [a[0]] + (a[1] as List) },
        "concat": core.&do_concat,
        "nth": core.&do_nth,
        "first": { a -> a[0] == null || a[0].size() == 0 ? null : a[0][0] },
        "rest": { a -> a[0] == null ? [] as List : a[0].drop(1) },
        "empty?": { a -> a[0] == null || a[0].size() == 0 },
        "count": { a -> a[0] == null ? 0 : a[0].size() },
        "apply": core.&do_apply,
        "map": { a -> a[1].collect { x -> a[0].call([x]) } },

        "conj": core.&do_conj,

        "meta": { a -> a[0].hasProperty("meta") ? a[0].getProperties().meta : null },
        "with-meta": { a -> def b = types.copy(a[0]); b.getMetaClass().meta = a[1]; b },
        "atom": { a -> new types.MalAtom(a[0]) },
        "atom?": { a -> a[0] instanceof types.MalAtom },
        "deref": { a -> a[0].value },
        "reset!": { a -> a[0].value = a[1] },
        "swap!": core.&do_swap_BANG
    ]
}

