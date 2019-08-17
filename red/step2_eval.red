Red [
	Title: "Step 2: eval"
	File:  %step2_eval.red
	Tabs:  4
]

#include %utils.red
#include %types.red
#include %reader.red
#include %printer.red

#include %readline.red

mal.read: func [
	str [string!]
][
	read-str str
]

eval-ast: func [
	ast
	env [map!]
	/local
		h k v
][
	case [
		mal-symbol? ast [
			either find/case env ast/name [
				find/case env ast/name
			][
				throw rejoi ["'" ast/name "' not found"]
			]
		]
		
		block? ast [
			map ast func [i][mal.eval i env]
		]
		
		mal-vector? ast [
			make mal-vector! [
				values: map ast/values func [i][mal.eval i env]
			]
		]
		
		map? ast [
			h: copy #()
			
			foreach [k v] to-block ast [
				put/case h k (mal.eval v env)
			]
			
			h
		]
		
		true [
			ast
		]
	]
]

mal.eval: func [
	ast
	env [map!]
	/local
		el
][
	unless block? ast [
		return eval-ast ast env
	]
	
	if empty? ast [
		return ast
	]
	
	el: copy seq-to-list (eval-ast ast env)
	el/1 mal.rest el
]

mal.print: func [
	val
	return: [string!]
][
	_pr_str :val true
]

rep: func [
	str     [string!]
	env     [map!]
	return: [string!]
][
	mal.print mal.eval (mal.read str) env
]

_repl-env: [
	_+:   func [args][args/1 + args/2]
	_-:   func [args][args/1 - args/2]
	_*:   func [args][args/1 * args/2]
	_div: func [args][args/1 / args/2]
]

repl-env: make map! reduce [
	"+" :_repl-env/_+
	"-" :_repl-env/_-
	"*" :_repl-env/_*
	"/" :_repl-env/_div
]

forever [
	ui: readline "user> "
	print rep ui repl-env
]