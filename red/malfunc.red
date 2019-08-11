Red [
	Title: "Mal function type"
	File:  %malfunc.red
	Tabs:  4
]

#include %env.red
#include %types.red

;-- predefine mal.eval
mal.eval: func [_1 _2][]

mal-func!: object [
	ast: env: params: is-macro: meta: none
	
	.init: func [
		"Make a MAL function"
		
		_ast    [block! object!] "Function body"
		_env    [object!]        "Outer scope"
		_params [block! object!] "Parameter names"
		return: [object!]
	][
		ast:      copy seq-to-list _ast
		env:      copy _env
		params:   copy seq-to-list _params
		is-macro: false
		self
	]
	
	.apply: func [
		args [block! object!]
		/local
			e [object!]
	][
		e: make mal-env! []
		mal.eval ast (e/.init env (map params func[i][i/name]) seq-to-list args)
	]
]

mal-func?: func [
	v
	return: [logic!]
][
	either object? :v [(class-of mal-func!) = (class-of v)][false]
]

mal.func?: func [
	v
	return: [logic!]
][
	either mal-func? :v [not v/is-macro][function? :v]
]

apply: func [
	f    [function! object!]
	args [block! object!]
][
	either mal-func? :f [f/.apply args][f seq-to-list args]
]