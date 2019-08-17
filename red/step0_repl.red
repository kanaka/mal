Red [
	Title: "Step 0: repl"
	File:  %step0_repl.red
	Tabs:  4
]

#include %readline.red

mal.read: func [
	str     [string!]
	return: [string!]
][
	str
]

mal.eval: func [
	str     [string!]
	return: [string!]
][
	str
]

mal.print: func [
	str     [string!]
	return: [string!]
][
	str
]

rep: func [
	str     [string!]
	return: [string!]
][
	mal.print mal.eval mal.read str
]

forever [
	ui: readline "user> "
	print rep ui
]