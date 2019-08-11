Red [
	Title: "Mal value formatter"
	File:  %printer.red
	Tabs:  4
]

#include %utils.red
#include %types.red
#include %malfunc.red

_pr_str: func [
	"Format a mal value as a string"
	
	obj
	print-readably [logic!]
	return:        [string!]
	/local
		_obj v elems
][
	case [
		function? :obj    ["#<native function>"]
		none? obj         ["nil"]
		obj =? mal-true!  ["true"]
		obj =? mal-false! ["false"]
		mal-symbol? obj   [obj/name]
		
		string? obj [
			_obj: copy obj
			case [
				(not empty? _obj) and (#"^(029e)" = first _obj) [
					rejoin [
						":"
						slice _obj 2 'end
					]
				]
				
				print-readably [
					replace/all _obj "\" "\\"
					replace/all _obj {"} {\"}
					replace/all _obj "^/" "\n"
					rejoin [{"} _obj {"}]
				]
				
				true [_obj]
			]
		]
		
		any-seq? obj [
			_obj: copy seq-to-list obj
			elems: copy []
			
			foreach v _obj [
				append/only elems _pr_str :v print-readably
			]
			
			either mal-vector? obj [
				rejoin ["[" elems "]"]
			][
				rejoin ["(" elems ")"]
			]
		]
		
		map? obj [
			rejoin [
				"{"
				to-block map obj func[k v][
					reduce [
						_pr_str k print-readably
						_pr_str v print-readably
					]
				]
				"}"
			]
		]
		
		mal-func? obj [
			rejoin [
				"(fn* "
				_pr_str obj/params true
				" "
				_pr_str obj/ast print-readably
				")"
			]
		]
		
		mal-atom? obj [rejoin ["(atom " _pr_str obj/value print-readably ")"]]
		true          [to-string obj]
	]
]