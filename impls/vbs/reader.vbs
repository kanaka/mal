Option Explicit

Function ReadString(strCode)
	Set ReadString = ReadForm(Tokenize(strCode))
End Function

Function Tokenize(strCode)
	Dim objRE
	Set objRE = New RegExp
	With objRE
		.Pattern = "[\s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""?|;.*|[^\s\[\]{}('""`,;)]*)" 
		.IgnoreCase = True
		.Global = True
	End With
	
	Dim objTokens, objMatches, objMatch
	Set objTokens = CreateObject("System.Collections.Queue")
	Set objMatches = objRE.Execute(strCode)
	Dim strToken
	For Each objMatch In objMatches
		strToken = objMatch.SubMatches(0)
		If Not Left(strToken, 1) = ";" Then
			objTokens.Enqueue strToken
		End If
	Next
	'MsgBox objTokens.Count
	'MsgBox """" & objTokens.peek & """"
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
	
	If objTokens.Count = 1 And objTokens.Peek() = "" Then
		Call objTokens.Dequeue()
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
		Call objTokens.Dequeue()
		
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
		Call objTokens.Dequeue()
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
		Set ReadForm = ReadAtom(objTokens)
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
		MsgBox "e"
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
		MsgBox "e"
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
			Set objKey = ReadForm(objTokens)
			Set objValue = ReadForm(objTokens)
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
					If IsNumeric(strAtom) Then
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
	If Right(strRaw, 1) <> """" Or Len(strRaw) < 2 Then
		MsgBox "e"
	End If
	
	Dim strTemp
	strTemp = Mid(strRaw, 2, Len(strRaw) - 2)
	Dim i
	i = 1
	'Dim strChar
	ParseString = ""
	While i <= Len(strTemp) - 1
		Select Case Mid(strTemp, i, 2)
			Case "\\"
				ParseString = ParseString & "\"
			Case "\n"
				ParseString = ParseString & vbCrLf
			Case "\"""
				ParseString = ParseString & """"
			Case Else
				ParseString = ParseString & Mid(strTemp, i, 1)
				i = i - 1
		End Select
		i = i + 2
	Wend
	
	If i <= Len(strTemp) Then
		' Last char is not processed.
		If Right(strTemp, 1) <> "\" Then
			ParseString = ParseString & Right(strTemp, 1)
		Else
			'TODO Error
			MsgBox "err"
		End If
	End If
End Function
