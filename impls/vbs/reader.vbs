Option Explicit

Function ReadString(strCode)
	Dim objTokens
	Set objTokens = Tokenize(strCode)
	Set ReadString = ReadForm(objTokens)
	If Not objTokens.AtEnd() Then
		Err.Raise vbObjectError, _
			"ReadForm", "extra token '" + objTokens.Current() + "'."
	End If
End Function

Class Tokens
	Private objQueue
	Private objRE

	Private Sub Class_Initialize
		Set objRE = New RegExp
		With objRE
			.Pattern = "[\s,]*" + _
				"(" + _
					"~@" + "|" + _
					"[\[\]{}()'`~^@]" + "|" + _
					"""(?:\\.|[^\\""])*""?" + "|" + _
					";.*" + "|" + _
					"[^\s\[\]{}('""`,;)]*" + _
				")"
			.IgnoreCase = True
			.Global = True
		End With

		Set objQueue = CreateObject("System.Collections.Queue")
	End Sub

	Public Function Init(strCode)
		Dim objMatches, objMatch
		Set objMatches = objRE.Execute(strCode)
		Dim strToken
		For Each objMatch In objMatches
			strToken = Trim(objMatch.SubMatches(0))
			If Not (Left(strToken, 1) = ";" Or strToken = "") Then
				objQueue.Enqueue strToken
			End If
		Next
	End Function

	Public Function Current()
		Current = objQueue.Peek()
	End Function

	Public Function MoveToNext()
		MoveToNext = objQueue.Dequeue()
	End Function

	Public Function AtEnd()
		AtEnd = (objQueue.Count = 0)
	End Function

	Public Function Count()
		Count = objQueue.Count
	End Function
End Class

Function Tokenize(strCode) ' Return objTokens
	Dim varResult
	Set varResult = New Tokens
	varResult.Init strCode
	Set Tokenize = varResult
End Function

Function ReadForm(objTokens) ' Return Nothing / MalType
	If objTokens.AtEnd() Then
		Set ReadForm = Nothing
		Exit Function
	End If

	Dim strToken
	strToken = objTokens.Current()

	Dim varResult
	If InStr("([{", strToken) Then
		Select Case strToken
			Case "("
				Set varResult = ReadList(objTokens)
			Case "["
				Set varResult = ReadVector(objTokens)
			Case "{"
				Set varResult = ReadHashmap(objTokens)
		End Select
	ElseIf InStr("'`~@", strToken) Then
		Set varResult = ReadSpecial(objTokens)
	ElseIf InStr(")]}", strToken) Then
		Err.Raise vbObjectError, _
			"ReadForm", "unbalanced parentheses."
	ElseIf strToken = "^" Then
		Set varResult = ReadMetadata(objTokens)
	Else
		Set varResult = ReadAtom(objTokens)
	End If

	Set ReadForm = varResult
End Function

Function ReadMetadata(objTokens)
	Dim varResult

	Call objTokens.MoveToNext()
	Dim objTemp
	Set objTemp = ReadForm(objTokens)
	Set varResult = NewMalList(Array( _
		NewMalSym("with-meta"), _
		ReadForm(objTokens), objTemp))

	Set ReadMetadata = varResult
End Function

Function ReadSpecial(objTokens)
	Dim varResult

	Dim strToken, strAlias
	strToken = objTokens.Current()
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
			Err.Raise vbObjectError, _
				"ReadSpecial", "unknown token '" & strAlias & "'."
	End Select

	Call objTokens.MoveToNext()
	Set varResult = NewMalList(Array( _
		NewMalSym(strAlias), _ 
		ReadForm(objTokens)))

	Set ReadSpecial = varResult
End Function

Function ReadList(objTokens)
	Dim varResult
	Call objTokens.MoveToNext()

	If objTokens.AtEnd() Then
		Err.Raise vbObjectError, _
			"ReadList", "unbalanced parentheses."
	End If

	Set varResult = NewMalList(Array())
	With varResult
		While objTokens.Count() > 1 And objTokens.Current() <> ")"
			.Add ReadForm(objTokens)
		Wend
	End With

	If objTokens.MoveToNext() <> ")" Then
		Err.Raise vbObjectError, _
			"ReadList", "unbalanced parentheses."
	End If

	Set ReadList = varResult
End Function

Function ReadVector(objTokens)
	Dim varResult
	Call objTokens.MoveToNext()

	If objTokens.AtEnd() Then
		Err.Raise vbObjectError, _
			"ReadVector", "unbalanced parentheses."
	End If

	Set varResult = NewMalVec(Array())
	With varResult
		While objTokens.Count() > 1 And objTokens.Current() <> "]"
			.Add ReadForm(objTokens)
		Wend
	End With

	If objTokens.MoveToNext() <> "]" Then
		Err.Raise vbObjectError, _
			"ReadVector", "unbalanced parentheses."
	End If

	Set ReadVector = varResult
End Function

Function ReadHashmap(objTokens)
	Dim varResult
	Call objTokens.MoveToNext()

	If objTokens.Count = 0 Then
		Err.Raise vbObjectError, _
			"ReadHashmap", "unbalanced parentheses."
	End If
	
	Set varResult = NewMalMap(Array(), Array())
	Dim objKey, objValue
	With varResult
		While objTokens.Count > 2 And objTokens.Current() <> "}"
			Set objKey = ReadForm(objTokens)
			Set objValue = ReadForm(objTokens)
			.Add objKey, objValue
		Wend
	End With
	
	If objTokens.MoveToNext() <> "}" Then
		Err.Raise vbObjectError, _
			"ReadHashmap", "unbalanced parentheses."
	End If
	
	Set ReadHashmap = varResult
End Function

Function ReadAtom(objTokens)
	Dim varResult

	Dim strAtom
	strAtom = objTokens.MoveToNext()

	Select Case strAtom
		Case "true"
			Set varResult = NewMalBool(True)
		Case "false"
			Set varResult = NewMalBool(False)
		Case "nil"
			Set varResult = NewMalNil()
		Case Else
			Select Case Left(strAtom, 1)
				Case ":"
					Set varResult = NewMalKwd(strAtom)
				Case """"
					Set varResult = NewMalStr(ParseString(strAtom))
				Case Else
					If IsNumeric(strAtom) Then
						Set varResult = NewMalNum(Eval(strAtom))
					Else
						Set varResult = NewMalSym(strAtom)
					End If
			End Select
	End Select

	Set ReadAtom = varResult
End Function

Function ParseString(strRaw)
	If Right(strRaw, 1) <> """" Or Len(strRaw) < 2 Then
		Err.Raise vbObjectError, _
			"ParseString", "unterminated string, got EOF."
	End If

	Dim strTemp
	strTemp = Mid(strRaw, 2, Len(strRaw) - 2)
	Dim i
	i = 1
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
			Err.Raise vbObjectError, _
				"ParseString", "unterminated string, got EOF."
		End If
	End If
End Function
