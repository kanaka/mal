Red [
	Title: "Step 3: env"
	File:  %step3_env.red
	Tabs:  4
]

#include %utils.red
#include %env.red
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
	env
	/local
		h k v
][
	case [
		mal-symbol? ast [
			env/.get ast/name
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
	env
	/local
		a1 let-env binds i j el
][
	unless block? ast [
		return eval-ast ast env
	]
	
	if empty? ast [
		return ast
	]
	
	a1: ast/1
	unless mal-symbol? a1 [
		throw "attempt to apply on non-symbol"
	]
	
	switch-case/default a1/name [
		"def!" [
			env/.set ast/2/name (mal.eval ast/3 env)
		]
		
		"let*" [
			let-env: make mal-env! []
			let-env/.init env [] []
			binds: copy seq-to-list ast/2
			
			foreach [i j] binds [
				let-env/.set i/name mal.eval j let-env
			]
			
			mal.eval ast/3 let-env
		]
	][
		el: copy seq-to-list (eval-ast ast env)
		el/1 mal.rest el
	]
]

mal.print: func [
	val
	return: [string!]
][
	_pr_str :val true
]

rep: func [
	str     [string!]
	env
	return: [string!]
][
	mal.print mal.eval (mal.read str) env
]

repl-env: make mal-env! []
repl-env/.init none [] []

repl-env/.set "+" func [args][args/1 + args/2]
repl-env/.set "-" func [args][args/1 - args/2]
repl-env/.set "*" func [args][args/1 * args/2]
repl-env/.set "/" func [args][args/1 / args/2]

forever [
	ui: readline "user> "
	print rep ui repl-env
]