Red [
	Title: "Mal core"
	File:  %core.red
	Tabs:  4
]

#include %utils.red
#include %types.red
#include %reader.red
#include %printer.red
#include %malfunc.red

#include %readline.red

wrap-tf: func [val][either val [mal-true!][mal-false!]]

mal-core: object [
	_ns: object [
		_=:           func [args][wrap-tf mal.equal? args/1 args/2]
		_throw:       func [args][throw to-string args/1]
		_nil?:        func [args][wrap-tf none? args/1]
		_true?:       func [args][wrap-tf args/1 =? mal-true!]
		_false?:      func [args][wrap-tf args/1 =? mal-false!]
		_string?:     func [args][wrap-tf mal.string? args/1]
		_symbol:      func [args][mal-symbol args/1]
		_symbol?:     func [args][wrap-tf mal-symbol? args/1]
		_keyword:     func [args][rejoin ["^(029e)" args/1]]
		_keyword?:    func [args][wrap-tf (string? args/1) and (not mal.string? args/1)]
		_number?:     func [args][wrap-tf number? args/1]
		_fn?:         func [args][wrap-tf mal.func? args/1]
		_macro?:      func [args][wrap-tf (mal-func? args/1) and args/1/is-macro]
		_pr-str:      func [args][rejoin map args func[i][_pr_str :i true]]
		_str:         func [args][to-string map args func[i][_pr_str :i false]]
		_prn:         func [args][print map args func[i][_pr_str :i true] none]
		_println:     func [args][print map args func[i][_pr_str :i false] none]
		_read-string: func [args][read-str args/1]
		_readline:    func [args][readline args/1]
		_slurp:       func [args][read to-file args/1]
		lt:           func [args][wrap-tf args/1 < args/2]
		le:           func [args][wrap-tf args/1 <= args/2]
		gt:           func [args][wrap-tf args/1 > args/2]
		ge:           func [args][wrap-tf args/1 >= args/2]
		plus:         func [args][args/1 + args/2]
		minus:        func [args][args/1 - args/2]
		times:        func [args][args/1 * args/2]
		div:          func [args][args/1 / args/2]
		_time-ms:     func [args][to-integer now]
		_list:        func [args][args]
		_list?:       func [args][wrap-tf block? args/1]
		_vector:      func [args][make mal-vector! [values: args]]
		_vector?:     func [args][wrap-tf mal-vector? args/1]
		_hash-map:    func [args][hash-map args]
		_map?:        func [args][wrap-tf map? args/1]
		_assoc:       func [args][mal.assoc (to-map seq-to-list args/1) (mal.rest args)]
		
		_dissoc: func [args /local k v][
			to-map copy collect [
				foreach [k v] to-block args/1 [
					if none? find/case/only (mal.rest args) k [
						keep reduce [k v]
					]
				]
			]
		]
		
		_get:         func [args][either none? args/1 [none][select/case args/1 args/2]]
		_contains?:   func [args][wrap-tf either none? args/1 [false][none? select/case args/1 args/2]]
		_keys:        func [args][keys-of args/1]
		_vals:        func [args][values-of args/1]
		_sequential?: func [args][wrap-tf any-seq? args/1]
		_cons:        func [args][append copy reduce [args/1] seq-to-list args/2]
		_concat:      func [args /local a][copy collect [foreach a args [keep seq-to-list a]]]
		_nth:         func [args][either args/2 <= length? (seq-to-list args/1) [pick seq-to-list args/1 (args/2 + 1)][throw "nth: index out of range"]]
		_first:       func [args][either none? args/1 [none][first seq-to-list args/1]]
		_rest:        func [args][either none? args/1 [copy []][mal.rest args/1]]
		_empty?:      func [args][wrap-tf empty? seq-to-list args/1]
		_count:       func [args][either none? args/1 [0][length? seq-to-list args/1]]
		_apply:       func [args][apply :args/1 append (copy slice args 2 -10) (seq-to-list last args)]
		
		_map: func [args /local out][
			out: map seq-to-list args/1 func[i][apply :args/2 reduce [:i]]
			either mal-vector? args/1 [make mal-vector! [values: out]][out]
		]
		
		_conj: func [args /local out][
			append (out: copy args/1) (copy mal.rest args)
			either mal-vector? args/1 [make mal-vector! [values: out]][out]
		]
		
		_seq: func [args /local i][
			case [
				none? args/1 [none]
				
				string? args/1 [
					either empty? args/1 [none][copy collect [foreach i args/1 [keep/only to-string i]]]
				]
				
				mal-vector? args/1 [
					either empty? args/1/values [none][make mal-vector! [values: copy args/1/values]]
				]
				
				block? args/1 [
					either empty? args/1 [none][copy args/1]
				]
				
				true [throw "seq: called on non-sequence"]
			]
		]
		
		_meta:      func [args][to-string args/1]
		_with-meta: func [args][args/1]
		_atom:      func [args][make mal-atom! [value: args/1]]
		_atom?:     func [args][wrap-tf mal-atom? args/1]
		_deref:     func [args][args/1/value]
		_reset!:    func [args][args/1/value: args/2]
		; maybe redo swap!
		_swap!:     func [args][args/1/value: apply args/2 append (copy reduce [args/1/value]) (mal.rest mal.rest args)]
	]
	
	ns: make map! reduce [
		"+"  :_ns/plus
		"-"  :_ns/minus
		"*"  :_ns/times
		"/"  :_ns/div
		"<"  :_ns/lt
		"<=" :_ns/le
		">"  :_ns/gt
		">=" :_ns/ge
	]
	
	foreach [k v] to-block _ns [
		if (first to-string k) = #"_" [
			put/case ns (slice to-string k 2 'end) :v
		]
	]
]