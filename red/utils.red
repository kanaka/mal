Red [
	Title: "Red utilities"
	File:  %utils.red
	Tabs:  4
]

map: func [
	b       [block! map!]
	f       [any-function!]
	return: [block!]
	/local
		i k v
][
	either map? b [
		copy to-map collect [foreach [k v] to-block b [keep f k :v]]
	][
		copy collect [foreach i copy b [keep/only f :i]]
	]
]

switch-case: func [
	v
	b [block!]
	/default
		d [block!]
	/local
		m m? t o
][
	m?: false
	
	foreach [m t] reduce b [
		if v == m [
			o: t
			m?: true
			break
		]
	]
	
	either m? [
		do o
	][
		either default [do d][none]
	]
]

slice: func [
	series  [block! paren! string! vector!]
	start   [integer!]
	stop    [integer! word!]
	return: [block! paren! string! vector!]
	/local
		i
][
	if stop == 'end [
		stop: length? series
	]
	
	if stop < 0 [
		stop: (length? series) + stop
	]
	
	if word? stop [throw "error"]
	
	to type? :series copy collect [
		i: start
		
		while [i <= stop][
			keep/only :series/:i
			i: i + 1
		]
	]
]