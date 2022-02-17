Class MalType
	Public Type_
	Public value_
End Class


'msgbox pr_str(read_str("(123 (456, 567))"))
'msgbox typename(CreateObject("System.Collections.ArrayList"))

Function read_str(str)
	set read_str=read_form(tokenize(str))
End Function

Function tokenize(str)
	Set oQueue = CreateObject("System.Collections.Queue")
	Set regEx = New RegExp
	regEx.Pattern = "[\s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""?|;.*|[^\s\[\]{}('""`,;)]*)" 
	regEx.IgnoreCase = True  
	regEx.Global = True
	Set Matches = regEx.Execute(str)
	For Each Match In Matches
		'msgbox Match.SubMatches(0)
		oQueue.Enqueue(Match.SubMatches(0))
	Next
	Set regEx = Nothing
	Set Matches = Nothing
	Set tokenize = oQueue
End Function

Function read_form(oQueue)
	If oQueue.Peek() = "(" Then
		set read_form = read_list(oQueue)
	Else
		set read_form = read_atom(oQueue)
	End If
End Function

Function read_list(oQueue)
	oQueue.Dequeue()

	set read_list = CreateObject("System.Collections.ArrayList")

	While oQueue.count <> 0 And oQueue.Peek() <> ")"
		read_list.Add read_form(oQueue)
	Wend
	If oQueue.count <> 0 Then
		oQueue.Dequeue()
	End If
End Function

Function read_atom(oQueue)
	atom = oQueue.Dequeue()
	if isnumeric(atom) Then
		set read_atom = new MalType
		read_atom.Type_ = "number"
		read_atom.value_ = atom
	else
		set read_atom = new MalType
		read_atom.Type_ = "symbol"
		read_atom.value_ = atom
	End If
End Function


'msgbox pr_str(read_str("1"))