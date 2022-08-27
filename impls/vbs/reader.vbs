Include "printer.vbs"

Class MalType
	Public Type
	Public Value
End Class

Function ReadString(strCode)
	Set ReadString = ReadForm(Tokenize(strCode))
End Function

Function Tokenize(strCode)
	Set objRE = New RegExp
	With objRE
		.Pattern = "[\s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""?|;.*|[^\s\[\]{}('""`,;)]*)" 
		.IgnoreCase = True
		.Global = True
	End With
	
	Set objTokens = CreateObject("System.Collections.Queue")
	Set objMatches = objRE.Execute(strCode)
	Dim strToken
	For Each objMatch In objMatches
		strToken = Match.SubMatches(0)
		If Not Left(strToken, 1) = ";" Then
			objTokens.Enqueue strToken
		End If
	Next
	
	Set Tokenize = objTokens
End Function

Function read_form_(oQueue)
	set read_form_=read_form(oQueue)
	'msgbox pr_str(read_form_),true
	if oQueue.Count > 0 then
		err.raise vbObjectError,"SyntaxError", "Extra data after form: " + oQueue.Dequeue
	end if
End Function

Function ReadForm(objTokens)
	If objTokens.Count = 0 Then
		Set ReadForm = Nothing
		Exit Function
	End If
	
	Dim strToken
	strToken = objTokens.Peek()
	
	If InStr("([{", strToken) Then
		Select Case strToken
			Case "("
				Set ReadForm = ReadList(oQueue)
			Case "["
				Set ReadForm = ReadVector(oQueue)
			Case "{"
				Set ReadForm = ReadHashmap(oQueue)
		End Select
	ElseIf InStr("'`~@", strToken) Then
		Select Case strToken
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

Function ReadList(objTokens)
	Call objTokens.Dequeue()
	
	If objTokens.Count = 0 Then
		'TODO
	End If
	
	Set ReadList = New MalType
	Set ReadList.Value = CreateObject("System.Collections.ArrayList")
	ReadList.Type = "List"

	With ReadList.Value
		While objTokens.Count > 1 And objTokens.Peek() <> ")"
			.Add ReadForm(objTokens)
		Wend
	End With
	
	If objTokens.Dequeue() <> ")" Then
		'TODO
		'Err.raise vbObjectError,"reader", "excepted '"+q+"', got EOF"
	End If
End Function

function ReadVector(objTokens)
	Call objTokens.Dequeue()
	
	If objTokens.Count = 0 Then
		'TODO
	End If
	
	Set ReadVector = New MalType
	Set ReadVector.Value = CreateObject("System.Collections.ArrayList")
	ReadVector.Type = "Vector"
	
	With ReadVector.Value
		While objTokens.Count > 1 And objTokens.Peek() <> "]"
			.Add ReadForm(objTokens)
		Wend
	End With
	
	If objTokens.Dequeue() <> "]" Then
		'TODO
		'err.raise vbObjectError,"reader", "excepted '"+q+"', got EOF"
	End If
End Function

Function ReadHashmap(objTokens)
	Call objTokens.Dequeue()
	
	If objTokens.Count < 2 Then
		'TODO
	End If
	
	Set ReadHashmap = New MalType
	Set ReadHashmap.Value = CreateObject("Scripting.Dictionary")
	ReadHashmap.Type = "Hashmap"
	
	Dim objKey, objValue
	With ReadHashmap.Value
		While objTokens.Count > 2 And objTokens.Peek() <> "}"
			Set objKey = ReadForm(oQueue)
			Set objValue = ReadForm(oQueue)
			.Add objKey, objValue
		Wend
	End With
	
	If objTokens.Dequeue() <> "}" Then
		'TODO
		'err.raise vbObjectError,"reader", "excepted '}', got EOF"
	End If
End Function

Function ReadAtom(objTokens)
	Dim strAtom
	strAtom = objTokens.Dequeue()
	
	'TODO
	if atom = "" then
		set read_atom = Nothing
	elseif isnumeric(atom) Then
		set read_atom = new MalType
		read_atom.Type_ = "number"
		read_atom.value_ = cdbl(atom)
		'msgbox "here"
	elseif atom = "true" or atom = "false" Then
		set read_atom = new MalType
		read_atom.Type_ = "boolean"
		read_atom.value_ = atom
	elseif left(atom,1) = """" Then
		if (not right(atom,1) = """") or len(atom) = 1 then err.raise vbObjectError,"reader", "Unterminated string, got EOF"
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
		if backslash then err.raise vbObjectError,"reader", "Unterminated string, got EOF"
		read_atom.value_ = str_tmp
	elseif atom = "nil" Then
		set read_atom = new MalType
		read_atom.Type_ = "null"
		read_atom.value_ = atom
	elseif left(atom,1) = ":" Then
		set read_atom = new MalType
		read_atom.Type_ = "keyword"
		read_atom.value_ = atom
	elseif left(atom,1) = ";" Then
		set read_atom = nothing
	else
		set read_atom = new MalType
		read_atom.Type_ = "symbol"
		read_atom.value_ = atom
	End If
End Function


Sub Include(sInstFile) 
	Dim oFSO, f, s 
	Set oFSO = CreateObject("Scripting.FileSystemObject")
    sInstFile = oFSO.GetParentFolderName(oFSO.GetFile(Wscript.ScriptFullName)) & "\" & sInstFile
	Set f = oFSO.OpenTextFile(sInstFile) 
	s = f.ReadAll 
	f.Close 
	Set f = Nothing
	Set oFSO = Nothing
	ExecuteGlobal s 
End Sub