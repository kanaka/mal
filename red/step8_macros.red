Red [
	Title: "Step 8: macros"
	File:  %step8_macros.red
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

is-pair?: func [
	obj
	return: [logic!]
][
	if none? obj [return false]
	all [
		any-seq? obj
		not empty? seq-to-list obj
	]
]

quasiquote: func [
	ast
	/local
		a1 a11
][
	either not is-pair? ast [
		reduce [
			mal-symbol "quote"
			ast
		]
	][
		a1: first seq-to-list ast
		
		case [
			mal-symbol? :a1 [
				if a1/name == "unquote" [
					return reduce (second seq-to-list ast)
				]
			]
			
			is-pair? :a1 [
				a11: first seq-to-list a1
				
				if mal-symbol? a11 [
					if a11/name == "splice-unquote" [
						return reduce [
							mal-symbol "concat"
							second a1
							quasiquote mal.rest ast
						]
					]
				]
			]
		]
			
		reduce [
			mal-symbol "cons"
			quasiquote :a1
			quasiquote mal.rest ast
		]
	]
]

is-macro-call?: func [
	ast
	env
	return: [logic!]
	/local
		mf
][
	if block? :ast [
		if mal-symbol? ast/1 [
			if env/.find ast/1/name [
				mf: env/.get ast/1/name
				if mal-func? :mf [return mf/is-macro]
			]
		]
	]
	false
]

macro-expand: func [
	ast
	env
][
	while [is-macro-call? ast env][
		ast: apply (env/.get ast/1/name) (mal.rest ast)
	]
	
	:ast
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
		
		ast: macro-expand ast env
		unless block? ast [
			break/return eval-ast ast env
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
			
			"quote" [
				break/return ast/2
			]
			
			"quasiquote" [
				ast: quasiquote ast/2 ;-- TCO
			]
			
			"defmacro!" [
				f: mal.eval ast/3 env
				f/is-macro: true
				break/return env/.set ast/2/name f
			]
			
			"macroexpand" [
				break/return macro-expand ast/2 env
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

argv: trim to-block system/script/args
unless empty? argv [
	argv: map argv func [a][to-string a]
]

foreach [k v] to-block core-ns [
	repl-env/.set k :v
]

repl-env/.set "eval" func [args][
	mal.eval :args/1 repl-env
]

repl-env/.set "*ARGV*" argv

rep "(def! not (fn* (a) (if a false true)))" repl-env
rep {(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\n)")))))} repl-env
rep {(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))} repl-env

unless empty? argv [
	rep rejoin [{(load-file "} argv/1 {")}] repl-env
	quit
]

forever [
	ui: readline "user> "
	print rep ui repl-env
]