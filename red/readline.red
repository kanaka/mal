Red [
	Title: "Readline"
	File:  %readline.red
	Tabs:  4
]

readline: func [
	prompt  [string!]
	return: [string! unset!]
	/local
		i [string!]
][
	either "q" == i: ask prompt [quit][i] 
]