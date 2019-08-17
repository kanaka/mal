Red [
	Title: "Step 5: tco"
	File:  %step5_tco.red
	Tabs:  4
]

#include %utils.red
#include %env.red
#include %types.red
#include %reader.red
#include %printer.red
#include %malfunc.red
#include %core.red

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
		a1 a1sym let-env binds i j f _env el args
][
	forever [
		unless block? ast [
			break/return eval-ast ast env
		]
		
		if empty? ast [
			break/return ast
		]
		
		a1: ast/1
		a1sym: either mal-symbol? a1 [a1/name]["__<*fn*>__"]
		
		switch-case/default a1sym [
			"def!" [
				break/return env/.set ast/2/name (mal.eval ast/3 env)
			]
			
			"let*" [
				let-env: make mal-env! []
				let-env/.init env [] []
				binds: copy seq-to-list ast/2
				
				foreach [i j] binds [
					let-env/.set i/name mal.eval j let-env
				]
				
				env: let-env
				ast: ast/3   ;-- TCO
			]
			
			"do" [
				eval-ast (slice ast 2 -1) env
				ast: last ast                 ;-- TCO
			]
			
			"if" [
				either any [
					none? mal.eval ast/2 env
					mal-false! =? mal.eval ast/2 env
				][
					either (length? ast) > 3 [
						ast: ast/4 ;-- TCO
					][
						break/return none
					]
				][
					ast: ast/3 ;-- TCO
				]
			]
			
			"fn*" [
				f: make mal-func! []
				break/return f/.init ast/3 env ast/2
			]
		][
			el: copy seq-to-list (eval-ast ast env)
			args: copy []
			if (length? el) > 1 [args: mal.rest el]
			
			either mal-func? :el/1 [
				_env: make mal-env! []
				env: _env/.init el/1/env (map el/1/params func[n][n/name]) args
				ast: el/1/ast                                                   ;-- TCO
			][
				break/return el/1 args
			]
		]
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

core-ns: mal-core/ns

foreach [k v] to-block core-ns [
	repl-env/.set k :v
]

rep "(def! not (fn* (a) (if a false true)))" repl-env

forever [
	ui: readline "user> "
	print rep ui repl-env
]