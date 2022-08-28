Option Explicit

Include "Const.vbs"
Include "Reader.vbs"
Include "Printer.vbs"
Include "Env.vbs"

Dim objEnv
Set objEnv = New Environment
objEnv.SetSelf objEnv
objEnv.SetOuter Nothing
objEnv.Add "+", GetRef("Add")
objEnv.Add "-", GetRef("Subtract")
objEnv.Add "*", GetRef("Multiply")
objEnv.Add "/", GetRef("Divide")


Sub CheckArgNum(objArgs, lngExpect)
	If objArgs.Value.Count - 1 <> 2 Then
		boolError = True
		strError = "wrong number of arguments"
		Call REPL()
	End If
End Sub

Function Add(objArgs)
	CheckArgNum objArgs, 2
	Set Add = New MalType
	Add.Type = TYPE_NUMBER
	Add.Value = objArgs.Value.Item(1).Value + objArgs.Value.Item(2).Value
End Function

Function Subtract(objArgs)
	CheckArgNum objArgs, 2
	Set Subtract = New MalType
	Subtract.Type = TYPE_NUMBER
	Subtract.Value = objArgs.Value.Item(1).Value - objArgs.Value.Item(2).Value
End Function

Function Multiply(objArgs)
	CheckArgNum objArgs, 2
	Set Multiply = New MalType
	Multiply.Type = TYPE_NUMBER
	Multiply.Value = objArgs.Value.Item(1).Value * objArgs.Value.Item(2).Value
End Function

Function Divide(objArgs)
	CheckArgNum objArgs, 2
	Set Divide = New MalType
	Divide.Type = TYPE_NUMBER
	Divide.Value = objArgs.Value.Item(1).Value / objArgs.Value.Item(2).Value
End Function


Call REPL()
Sub REPL()
	Dim strCode, strResult
	While True
		If boolError Then
			WScript.StdErr.WriteLine "ERROR: " & strError
			boolError = False
		End If
		WScript.StdOut.Write("user> ")
		On Error Resume Next
		strCode = WScript.StdIn.ReadLine()
		If Err.Number <> 0 Then WScript.Quit 0
		On Error Goto 0
		WScript.Echo REP(strCode)
	Wend
End Sub

Function Read(strCode)
	Set Read = ReadString(strCode)
End Function

Function Evaluate(objCode, objEnv)
	Dim i
	If TypeName(objCode) = "Nothing" Then
		Call REPL()
	End If
	
	If objCode.Type = TYPE_LIST Then
		If objCode.Value.Count = 0 Then
			Set Evaluate = objCode
			Exit Function
		End If
		
		Dim objSymbol
		Set objSymbol = Evaluate(objCode.Value.Item(0), objEnv)
		If TypeName(objSymbol) = "MalType" Then
			'MsgBox TypeName(objCode.value)
			Select Case objSymbol.Value
				Case "def!"
					CheckArgNum objCode, 2
					CheckSymbol objCode.Value.Item(1)
					'MsgBox 2
					objEnv.Add objCode.Value.Item(1).Value, _
						Evaluate(objCode.Value.Item(2), objEnv)
					'MsgBox 3
					Set Evaluate = objEnv.Get(objCode.Value.Item(1).Value)
				Case "let*"
					Dim objNewEnv
					Set objNewEnv = New Environment
					objNewEnv.SetSelf objNewEnv
					objNewEnv.SetOuter objEnv
					CheckArgNum objCode, 2
					CheckListOrVector objCode.Value.Item(1)
					CheckEven objCode.Value.Item(1).Value.Count
					With objCode.Value.Item(1).Value
						For i = 0 To .Count - 1 Step 2
							CheckSymbol .Item(i)
							objNewEnv.Add .Item(i).Value, _
								Evaluate(.Item(i + 1), objNewEnv)
						Next
					End With
					Set Evaluate = Evaluate(objCode.Value.Item(2), objNewEnv)
			End Select
		Else
			Set Evaluate = objSymbol(EvaluateAST(objCode, objEnv))
		End If
	Else
		Set Evaluate = EvaluateAST(objCode, objEnv)
	End If
End Function

Sub CheckEven(lngNum)
	If lngNum Mod 2 <> 0 Then
		boolError = True
		strError = "not a even number"
		Call REPL()
	End If	
End Sub

Sub CheckList(objMal)
	If objMal.Type <> TYPE_LIST Then
		boolError = True
		strError = "neither a list nor a vector"
		Call REPL()
	End If
End Sub

Sub CheckListOrVector(objMal)
	If objMal.Type <> TYPE_LIST And objMal.Type <> TYPE_VECTOR Then
		boolError = True
		strError = "not a list"
		Call REPL()
	End If
End Sub

Sub CheckSymbol(objMal)
	If objMal.Type <> TYPE_SYMBOL Then
		boolError = True
		strError = "not a symbol"
		Call REPL()
	End If
End Sub

Function EvaluateAST(objCode, objEnv)
	If TypeName(objCode) = "Nothing" Then
		MsgBox "Nothing2"
	End If
	
	Dim objResult, i
	Select Case objCode.Type
		Case TYPE_SYMBOL
			Select Case objCode.Value
				Case "def!"
					Set objResult = objCode
				Case "let*"
					Set objResult = objCode
				Case Else
					Set objResult = objEnv.Get(objCode.Value)	
			End Select
		Case TYPE_LIST
			For i = 0 To objCode.Value.Count - 1
				Set objCode.Value.Item(i) = Evaluate(objCode.Value.Item(i), objEnv)
			Next
			Set objResult = objCode
		Case TYPE_VECTOR
			For i = 0 To objCode.Value.Count - 1
				Set objCode.Value.Item(i) = Evaluate(objCode.Value.Item(i), objEnv)
			Next
			Set objResult = objCode
		Case TYPE_HASHMAP
			Dim arrKeys
			arrKeys = objCode.Value.Keys
			For i = 0 To objCode.Value.Count - 1
				Set objCode.Value.Item(arrKeys(i)) = _
					Evaluate(objCode.Value.Item(arrKeys(i)), objEnv)
			Next
			Set objResult = objCode
		Case Else
			Set objResult = objCode
	End Select
	Set EvaluateAST = objResult
End Function

Function Print(objCode)
	Print = PrintMalType(objCode, True)
End Function

Function REP(strCode)
	REP = Print(Evaluate(Read(strCode), objEnv))
End Function

Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub