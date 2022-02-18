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
		if not left(Match.SubMatches(0), 1) = ";" then
			oQueue.Enqueue(Match.SubMatches(0))
		End if
	Next
	Set regEx = Nothing
	Set Matches = Nothing
	Set tokenize = oQueue
End Function

Function read_form(oQueue)
	if oQueue.Count = 0 then
		Set read_form = Nothing
		exit function
	end if
	If oQueue.Peek() = "(" or oQueue.Peek() = "[" or oQueue.Peek() = "{" Then
		set read_form = read_list(oQueue)
	elseif oQueue.Peek() = "'" or oQueue.Peek() = "`" or oQueue.Peek() = "~" or oQueue.Peek() = "~@" or oQueue.Peek = "@" then
		select case oQueue.Dequeue()
			case "'"
				s = "quote"
			case "`"
				s = "quasiquote"
			case "~"
				s = "unquote"
			case "~@"
				s = "splice-unquote"
			case "@"
				s = "deref"
		end select
		set o = new MalType
		o.Type_ = "symbol"
		o.value_ = s
		set l = new MalType
		l.Type_ = "list()"
		set l.value_ = CreateObject("System.Collections.ArrayList")
		l.value_.Add(o)
		l.value_.Add(read_form(oQueue))
		set read_form = l
	elseif oQueue.Peek() = ")" or oQueue.Peek() = "]" or oQueue.Peek() = "}" then
		Set read_form = Nothing
		err.Raise vbObjectError, "read_form", "unbalanced parentheses"
	elseif oQueue.Peek() = "^" then
		oQueue.Dequeue()
		set o = new MalType
		o.Type_ = "symbol"
		o.value_ = "with-meta"
		set l = new MalType
		l.Type_ = "list()"
		set l.value_ = CreateObject("System.Collections.ArrayList")
		l.value_.Add(o)
		set tmp = read_form(oQueue)
		l.value_.Add(read_form(oQueue))
		l.value_.Add(tmp)
		set read_form = l
	Else
		set read_form = read_atom(oQueue)
	End If
End Function

Function read_list(oQueue)
	p = oQueue.Dequeue()
	if p = "(" Then
		q = ")"
	elseif p = "[" then
		q = "]"
	elseif p = "{" then
		q = "}"
	end if

	set read_list = new MalType
	set read_list.value_ = CreateObject("System.Collections.ArrayList")
	read_list.type_ = "list"+p+q

	While oQueue.count > 1 And oQueue.Peek() <> q
		read_list.value_.Add read_form(oQueue)
	Wend
	If oQueue.Dequeue() <> q Then
		err.raise vbObjectError,"reader", "excepted '"+q+"', got EOF"
	End If
End Function

Function read_atom(oQueue)
	atom = oQueue.Dequeue()
	if isnumeric(atom) Then
		set read_atom = new MalType
		read_atom.Type_ = "number"
		read_atom.value_ = atom
	elseif atom = "true" or atom = "false" Then
		set read_atom = new MalType
		read_atom.Type_ = "boolean"
		read_atom.value_ = atom
	elseif left(atom,1) = """" Then
		if (not right(atom,1) = """") or len(atom) = 1 then err.raise vbObjectError,"reader", "Unterminated string"
		set read_atom = new MalType
		read_atom.Type_ = "string"
		str_tmp = ""
		for i = 2 to len(atom) - 1
			if backslash then
				backslash = False
				'msgbox backslash
				if mid(atom,i,1) = "n" then 
					str_tmp = str_tmp + vbnewline
				elseif mid(atom,i,1) = "\" then 
					str_tmp = str_tmp + "\"
				elseif mid(atom,i,1) = """" then 
					str_tmp = str_tmp + """"
				end if
			else 
				if mid(atom,i,1) = "\" then 
					backslash = True
				else
					str_tmp = str_tmp + mid(atom,i,1)
				end if
			end if
		next
		if backslash then err.raise vbObjectError,"reader", "Unterminated string"
		read_atom.value_ = """" + str_tmp + """"
	elseif atom = "nil" Then
		set read_atom = new MalType
		read_atom.Type_ = "null"
		read_atom.value_ = atom
	elseif left(atom,1) = ";" Then
		set read_atom = nothing
	else
		set read_atom = new MalType
		read_atom.Type_ = "symbol"
		read_atom.value_ = atom
	End If
End Function


