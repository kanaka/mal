Red [
	Title: "Mal env"
	File:  %env.red
	Tabs:  4
]

#include %utils.red

mal-env!: object [
	data: outer: none
	
	.init: func [
		"Make a MAL env"
		
		_outer  [object! none!] "Possible outer scope"
		binds   [block!]        "Local variable names"
		exprs   [block!]        "Local variable values"
		return: [object!]
		/local
			i [integer!]
	][
		data: copy #()
		outer: either none? _outer [none][copy _outer]
		
		repeat i length? binds [
			either binds/:i = "&" [
				data/(binds/(i + 1)): slice exprs i 'end
				break
			][
				data/(binds/:i): exprs/:i
			]
		]
		
		self
	]
	
	.find: func [
		"Determine if a key exists"
		
		key     [string!]
		return: [object! none!]
	][
		case [
			not none? (select/case data key) [self]
			not none? outer      [outer/.find key]
			true                 [none]
		]
	]
	
	.set: func [
		"Set a key"
		
		key [string!]
		value
	][
		extend/case data make map! reduce [key :value]
		return :value
	]
	
	.get: func [
		"Get a key"
		
		key [string!]
		/local
			e [object! none!]
	][
		if none? e: .find key [
			throw rejoin ["'" key "' not found"]
		]
		select/case e/data key
	]
]