Option Explicit

Function ReadString(strCode)
	Set ReadString = ReadForm(Tokenize(strCode))
End Function

Class Tokens
	Private strRaw, objTokens
	Private objRE

	Private Sub Class_Initialize
		Set objRE = New RegExp
		With objRE
			.Pattern = "[\s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""?|;.*|[^\s\[\]{}('""`,;)]*)"
			.IgnoreCase = True
			.Global = True
		End With

		Set objTokens = CreateObject("System.Collections.Queue")
	End Sub

	Public Function Init(strCode)
		strRaw = strCode

		Dim objMatches, objMatch
		Set objMatches = objRE.Execute(strCode)
		Dim strToken
		For Each objMatch In objMatches
			strToken = Trim(objMatch.SubMatches(0))
			If Not (Left(strToken, 1) = ";" Or strToken = "") Then
				' Drop comments
				objTokens.Enqueue Trim(strToken)
			End If
		Next
	End Function

	Public Function Current()
		Current = objTokens.Peek()
	End Function

	Public Function MoveToNext()
		MoveToNext = objTokens.Dequeue()
	End Function

	Public Function AtEnd()
		AtEnd = (objTokens.Count = 0)
	End Function

	Public Function Count()
		Count = objTokens.Count
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
			"ReadForm", "unbalanced parentheses"
	ElseIf strToken = "^" Then
		Set varResult = ReadMetadata(objTokens)
	Else
		Set varResult = ReadAtom(objTokens)
	End If

	If Not objTokens.AtEnd() Then
		'Err.Raise vbObjectError, _
		'	"ReadForm", "extra token(s): " + objTokens.Current()
	End If

	Set ReadForm = varResult
End Function

Function ReadMetadata(objTokens)
	Dim varResult

	Call objTokens.MoveToNext()
	Dim objTmp
	Set objTmp = ReadForm(objTokens)
	Set varResult = NewMalList(Array( _
		NewMalType(TYPES.SYMBOL, "with-meta"), _
		ReadForm(objTokens), objTmp))

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
				"ReadSpecial", "unknown token " & strAlias
	End Select

	Call objTokens.MoveToNext()
	Set varResult = NewMalList(Array( _
		NewMalType(TYPES.SYMBOL, strAlias), _ 
		ReadForm(objTokens)))
	Set ReadSpecial = varResult
End Function

Function ReadList(objTokens)
	Dim varResult
	Call objTokens.MoveToNext()

	If objTokens.AtEnd() Then
		Err.Raise vbObjectError, _
			"ReadList", "unbalanced parentheses"
	End If

	Set varResult = NewMalList(Array())
	With varResult
		While objTokens.Count() > 1 And objTokens.Current() <> ")"
			.Add ReadForm(objTokens)
		Wend
	End With

	If objTokens.MoveToNext() <> ")" Then
		Err.Raise vbObjectError, _
			"ReadList", "unbalanced parentheses"
	End If

	Set ReadList = varResult
End Function

Function ReadVector(objTokens)
	Dim varResult
	Call objTokens.MoveToNext()

	If objTokens.AtEnd() Then
		Err.Raise vbObjectError, _
			"ReadVector", "unbalanced parentheses"
	End If

	Set varResult = NewMalVector(Array())
	With varResult
		While objTokens.Count() > 1 And objTokens.Current() <> "]"
			.Add ReadForm(objTokens)
		Wend
	End With

	If objTokens.MoveToNext() <> "]" Then
		Err.Raise vbObjectError, _
			"ReadVector", "unbalanced parentheses"
	End If

	Set ReadVector = varResult
End Function

Function ReadHashmap(objTokens)
	Dim varResult
	Call objTokens.MoveToNext()

	If objTokens.Count = 0 Then
		Err.Raise vbObjectError, _
			"ReadHashmap", "unbalanced parentheses"
	End If
	Set varResult = NewMalHashmap(Array(), Array())

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
			"ReadHashmap", "unbalanced parentheses"
	End If

	Set ReadHashmap = varResult
End Function

Function ReadAtom(objTokens)
	Dim varResult

	Dim strAtom
	strAtom = objTokens.MoveToNext()

	Select Case strAtom
		Case "true"
			Set varResult = NewMalType(TYPES.BOOLEAN, True)
		Case "false"
			Set varResult = NewMalType(TYPES.BOOLEAN, False)
		Case "nil"
			Set varResult = NewMalType(TYPES.NIL, Empty)
		Case Else
			Select Case Left(strAtom, 1)
				Case ":"
					Set varResult = NewMalType(TYPES.KEYWORD, strAtom)
				Case """"
					Set varResult = NewMalType(TYPES.STRING, ParseString(strAtom))
				Case Else
					If IsNumeric(strAtom) Then
						Set varResult = NewMalType(TYPES.NUMBER, Eval(strAtom))
					Else
						Set varResult = NewMalType(TYPES.SYMBOL, strAtom)
					End If
			End Select
	End Select

	Set ReadAtom = varResult
End Function

Function ParseString(strRaw)
	If Right(strRaw, 1) <> """" Or Len(strRaw) < 2 Then
		Err.Raise vbObjectError, _
			"ParseString", "unterminated string, got EOF"
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
				"ParseString", "unterminated string, got EOF"
		End If
	End If
End Function
