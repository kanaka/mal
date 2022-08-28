Option Explicit

Include "Core.vbs"
Include "Reader.vbs"
Include "Printer.vbs"

Dim objEnv
Set objEnv = CreateObject("Scripting.Dictionary")
objEnv.Add "+", GetRef("Add")
objEnv.Add "-", GetRef("Subtract")
objEnv.Add "*", GetRef("Multiply")
objEnv.Add "/", GetRef("Divide")

Sub CheckArgNum(objArgs, lngExpect)
	If objArgs.Value.Count - 1 <> lngExpect Then
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
	If TypeName(objCode) = "Nothing" Then
		Call REPL()
	End If
	
	If objCode.Type = TYPE_LIST Then
		If objCode.Value.Count = 0 Then
			Set Evaluate = objCode
			Exit Function
		End If
		
		Set Evaluate = EvaluateAST(objCode, objEnv)
		Set Evaluate = Evaluate.Value.Item(0)(Evaluate)
	Else
		Set Evaluate = EvaluateAST(objCode, objEnv)
	End If
End Function

Function EvaluateAST(objCode, objEnv)
	Dim objResult, i
	Select Case objCode.Type
		Case TYPE_SYMBOL
			If objEnv.Exists(objCode.Value) Then
				Set objResult = objEnv(objCode.Value)
			Else
				boolError = True
				strError = "symbol not found"
				Call REPL()
			End If
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