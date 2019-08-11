Red [
	Title: "Mal types"
	File:  %types.red
	Tabs:  4
]

#include %utils.red
#include %env.red

;-- types
mal-true!:  object []
mal-false!: object []

mal-symbol!: object [
	name: none
]

mal-symbol?: func [
	v
	return: [logic!]
][
	either object? :v [(class-of mal-symbol!) = (class-of v)][false]
]

mal-symbol: func [
	n       [string!]
	return: [object!]
][
	make mal-symbol! [name: n]
]


mal-vector!: object [
	values: []
]

mal-vector?: func [
	v
	return: [logic!]
][
	either object? :v [(class-of mal-vector!) = (class-of v)][false]
]

any-seq?: func [
	v
	return: [logic!]
][
	(block? :v) or (mal-vector? :v)
]

seq-to-list: func [
	v       [block! object!]
	return: [block!]
][
	case [
		block? :v      [v]
		mal-vector? :v [v/values]
		true           [throw rejoin ["Invalid seq: " v]]
	]
]


mal-atom!: object [
	value: none
	
	.init: func [
		"Make a MAL atom object"
		
		val
		return: [object!]
	][
		either any [
			number?   :val
			function? :val
			none?     :val
		] [value: :val][value: copy val]
		
		self
	]
]

mal-atom?: func [
	obj
	return: [logic!]
][
	either object? :obj [(class-of mal-atom!) = (class-of obj)][false]
]


;-- other functions
mal.string?: func [
	obj
	return: [logic!]
][
	either string? :obj [(first obj) <> #"^(029e)"][false]
]

mal.rest: func [
	obj     [block! object!]
	return: [block! object!]
][
 	either block? obj [
		slice obj 2 'end
	][
		make mal-vector! [values: slice obj/values 2 'end]
	]
]

mal.assoc: func [
	d       [map!]
	kvs     [block! object!]
	return: [map!]
][
	foreach [k v] seq-to-list kvs [
		put/case d to-string k :v
	]
	return d
]

hash-map: func [kvs][mal.assoc copy #() kvs]

mal.equal?: func [
	a
	b
	return: [logic!]
	/local
		i
][
	unless any [
		(type? a)       =?  (type? b)
		(block? a)      and (block? b)
		(mal-vector? a) and (mal-vector? b)
		(number? a)     and (number? b)
		(mal.string? a) and (mal.string? b)
	][
		return false
	]
	
	case [
		(a =? mal-true!) or (a =? mal-false!) [a =? b]
		
		block? a [
			case [
				empty? a                   [true]
				(length? a) <> (length? b) [false]
				true [
					all copy collect [
						repeat i length? a [
							keep mal.equal? a/:i b/:i
						]
					]
				]
			]
		]
		
		mal-vector? a [mal.equal? a/values b/values]
		true          [a == b]
	]
]