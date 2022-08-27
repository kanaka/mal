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

'Function read_form_(oQueue)
'	set read_form_=read_form(oQueue)
'	'msgbox pr_str(read_form_),true
'	if oQueue.Count > 0 then
'		err.raise vbObjectError,"SyntaxError", "Extra data after form: " + oQueue.Dequeue
'	end if
'End Function

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
				Set ReadForm = ReadList(objTokens)
			Case "["
				Set ReadForm = ReadVector(objTokens)
			Case "{"
				Set ReadForm = ReadHashmap(objTokens)
		End Select
	ElseIf InStr("'`~@", strToken) Then
		Dim strAlias
		Select Case strToken
			Case "'"
				strAlias = "quote"
			Case "`"
				strAlias = "quasiquote"
			Case "~"
				strAlias = "unquote"
			Case "~@"
				strAlias = "splice-unquote"
			Case "@"
				strAlias = "deref"
			Case Else
				'TODO
		End Select
		
		Set ReadForm = New MalType
		ReadForm.Type = TYPE_LIST
		Set ReadForm.Value = CreateObject("System.Collections.ArrayList")
		ReadForm.Value.Add New MalType
		ReadForm.Value.Item(0).Type = TYPE_SYMBOL
		ReadForm.Value.Item(0).Value = strAlias
		ReadForm.Value.Add ReadForm(objTokens)
	'TODO
	'ElseIf oQueue.Peek() = ")" or oQueue.Peek() = "]" or oQueue.Peek() = "}" then
	'	Set read_form = Nothing
	'	err.Raise vbObjectError, "read_form", "unbalanced parentheses"
	ElseIf strToken = "^" Then
		oQueue.Dequeue()
		Set ReadForm = New MalType
		ReadForm.Type = TYPE_LIST
		Set ReadForm.Value = CreateObject("System.Collections.ArrayList")
		ReadForm.Value.Add New MalType
		ReadForm.Value.Item(0).Type = TYPE_SYMBOL
		ReadForm.Value.Item(0).Value = "with-meta"
		Dim objTemp
		Set objTemp = ReadForm(objTokens)
		ReadForm.Value.Add ReadForm(objTokens)
		ReadForm.Value.Add objTemp
	Else
		Set read_form = read_atom(oQueue)
	End If
End Function

Function ReadList(objTokens)
	Call objTokens.Dequeue()
	
	If objTokens.Count = 0 Then
		'TODO
	End If
	
	Set ReadList = New MalType
	Set ReadList.Value = CreateObject("System.Collections.ArrayList")
	ReadList.Type = TYPE_LIST

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
	ReadVector.Type = TYPE_VECTOR
	
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
	ReadHashmap.Type = TYPE_HASHMAP
	
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
	
	Dim objAtom
	Set objAtom = New MalType
	Select Case strAtom
		Case "true"
			objAtom.Type = TYPE_BOOLEAN
			objAtom.Value = True
		Case "false"
			objAtom.Type = TYPE_BOOLEAN
			objAtom.Value = False
		Case "nil"
			objAtom.Type = TYPE_NIL
			objAtom.Value = Null
		Case Else
			Select Case Left(strAtom, 1)
				Case ":"
					objAtom.Type = TYPE_KEYWORD
					objAtom.Value = strAtom
				Case """"
					'TODO check string
					'if (not right(atom,1) = """") or len(atom) = 1 then err.raise vbObjectError,"reader", "Unterminated string, got EOF"
					objAtom.Type = TYPE_STRING
					objAtom.Value = ParseString(strAtom)
				Case Else
					If IsNumeric(strAtom)
						objAtom.Type = TYPE_NUMBER
						objAtom.Value = Eval(strAtom)
					Else
						objAtom.Type = TYPE_SYMBOL
						objAtom.Value = strAtom
					End If
			End Select
	End Select
	
	Set ReadAtom = objAtom
End Function

Function ParseString(strRaw)
	ParseString = strRaw
	'TODO
'	Dim atom
'	atom=strAtom
'	if atom = "" then
'		set read_atom = Nothing
'	elseif left(atom,1) = """" Then
'		set read_atom = new MalType
'		read_atom.Type_ = "string"
'		str_tmp = ""
'		for i = 2 to len(atom) - 1
'			if backslash then
'				backslash = False
'				'msgbox backslash
'				if mid(atom,i,1) = "n" then 
'					str_tmp = str_tmp + vbnewline
'				elseif mid(atom,i,1) = "\" then 
'					str_tmp = str_tmp + "\"
'				elseif mid(atom,i,1) = """" then 
''					str_tmp = str_tmp + """"
'				end if
'			else 
'				if mid(atom,i,1) = "\" then 
'					backslash = True
'				else
'					str_tmp = str_tmp + mid(atom,i,1)
'				end if
'			end if
'		next
'		if backslash then err.raise vbObjectError,"reader", "Unterminated string, got EOF"
'		read_atom.value_ = str_tmp
	'elseif left(atom,1) = ";" Then	
	'	set read_atom = nothing
	'else
	'	set read_atom = new MalType
	'	read_atom.Type_ = "symbol"
	'	read_atom.value_ = atom
	'End If
End Function
