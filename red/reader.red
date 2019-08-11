Red [
	Title: "Mal lexer/reader"
	File:  %reader.red
	Tabs:  4
]

#include %types.red

mal-reader!: object [
	tokens: position: none
	
	.init: func [
		"Make a MAL reader object"
		
		toks    [block! object!] "List of tokens"
		return: [object!]
	][
		tokens: copy seq-to-list toks
		position: 1
		self
	]
	
	.next: does [
		position: position + 1
		tokens/(position - 1)
	]
	
	.peek: does [
		either (length? tokens) >= position [
			tokens/:position
		][
			none
		]
	]
]

tokenize: func [
	str     [string!]
	return: [block!]
	/local
		tokens t out
		r=ws r=ws+cm r=sym r=sp_sym r=dq+bs r=!dq+bs r=!nl "Rule variables"
][
	r=ws:     charset reduce [space tab newline lf cr]
	r=ws+cm:  union r=ws charset comma
	r=sym:    union r=ws charset "[]{}()'^"`@,;"
	r=sp_sym: charset "[]{}()'`~^^@"
	r=dq+bs:  charset {"\}
	r=!dq+bs: complement r=dq+bs
	r=!nl:    complement charset reduce [newline cr lf]
	
	tokens: copy parse str [
		collect [
			some [
				any r=ws+cm
				keep [
					  "~@"
					| r=sp_sym
					| {"} any ["\"skip | r=!dq+bs] {"}
					| ";" any r=!nl
					| to r=sym
				]
			]
		]
	]
	
	return copy collect [
		foreach t tokens [
			unless #";" = first to-string t [
				keep/only t
			]
		]
	]
]

read-atom: func [
	rdr [object!] "The reader! object"
	/local
		res token
		r=digit r=dq+bs r=!dq+bs r=!dq                                      "Rule variables"
		m=true m=false m=nil m=int m=dec m=str m=bad_str m=keyword m=symbol "Match variables"
][
	m=true: m=false: m=nil: m=int: m=dec: m=str: m=bad_str: m=keyword: m=symbol: none
	
	r=digit:  charset "0123456789"
	r=dq+bs:  charset {"\}
	r=!dq+bs: complement r=dq+bs
	r=!dq:    complement charset {"}
	
	res: parse/case to-string token: rdr/.next [
		[
			  ahead ["true" end]  copy m=true    "true"
			| ahead ["false" end] copy m=false   "false"
			| ahead ["nil" end]   copy m=nil     "nil"
			|                     copy m=dec     [opt "-" some r=digit "." any r=digit]
			|                     copy m=int     [opt "-" some r=digit]
			| {"}                 copy m=str     [any ["\"skip | r=!dq+bs]] {"}
			| {"}                 copy m=bad_str [any ["\"skip | r=!dq+bs]]
			| ":"                 copy m=keyword [any skip]
			|                     copy m=symbol  [any r=!dq end]
		]
		end
	]
	
	if res [
		case [
			m=true  [return mal-true!]
			m=false [return mal-false!]
			m=nil   [return none]
			m=int   [return to-integer m=int]
			m=dec   [return to-float m=dec]
			m=str   [
				replace/all      m=str "\\" "^(029e)"
				replace/all      m=str {\"} {"}
				replace/all/case m=str "\n" "^/"
				replace/all      m=str #"^(029e)" #"\"
				return m=str
			]
			m=bad_str [throw {read-atom: expected '\"', but got EOF}]
			m=keyword [return rejoin ["^(029e)" m=keyword]]
			m=symbol  [return mal-symbol m=symbol]
		]
	]
	
	throw rejoin ["read-atom: invalid token `" token "`"]
]

read-list: func [
	rdr     [object!] "The reader! object"
	start   [char!]   "Opening token"
	stop    [char!]   "Closing token"
	return: [block!]
	/local
		token ast
][
	ast: copy []
	token: rdr/.next
	
	if token <> start [
		throw rejoin ["expected '" start "'"]
	]
	
	while [(token: rdr/.peek) <> stop][
		if none? token [
			probe ast
			throw rejoin ["expected '" stop "', got EOF"]
		]
		
		append/only ast read-form rdr
	]
	
	rdr/.next
	
	return ast
]

read-form: func [
	rdr [object!] "The reader object"
	/local
		token meta
][
	token: to-string rdr/.peek
	
	switch/default first token [
		#"'"  [rdr/.next reduce [mal-symbol "quote" read-form rdr]]
		#"`"  [rdr/.next reduce [mal-symbol "quasiquote" read-form rdr]]
		#"~"  [rdr/.next either token = "~@" [reduce [mal-symbol "splice-unquote" read-form rdr]][reduce [mal-symbol "unquote" read-form rdr]]]
		#"^^" [rdr/.next meta: read-form rdr reduce [mal-symbol "with-meta" read-form rdr meta]]
		#"@"  [rdr/.next reduce [mal-symbol "deref" read-form rdr]]
		#")"  [throw "unexpected ')'"]
		#"("  [read-list rdr #"(" #")"]
		#"]"  [throw "unexpected ']'"]
		#"["  [make mal-vector! [values: read-list rdr #"[" #"]"]]
		#"}"  [throw "unexpected '}'"]
		#"{"  [hash-map read-list rdr #"{" #"}"]
	][
		read-atom rdr
	]
]

read-str: func [
	str [string!]
	/local
		out tokens
][
	tokens: copy tokenize str
	
	if empty? tokens [throw "empty token"]
	
	out: make mal-reader! []
	
	read-form out/.init tokens
]