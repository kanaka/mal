Option Explicit

Include "Const.vbs"
Include "Reader.vbs"
Include "Printer.vbs"
Include "Env.vbs"

Dim objEnv
Set objEnv = New Environment
objEnv.SetSelf objEnv
objEnv.SetOuter Nothing
objEnv.Add "+", NewLambda(GetRef("Add"))
objEnv.Add "-", NewLambda(GetRef("Subtract"))
objEnv.Add "*", NewLambda(GetRef("Multiply"))
objEnv.Add "/", NewLambda(GetRef("Divide"))
objEnv.Add "def!", NewSpecialForm("def!")
objEnv.Add "let*", NewSpecialForm("let*")
objEnv.Add "do", NewSpecialForm("do")
objEnv.Add "if", NewSpecialForm("if")
objEnv.Add "fn*", NewSpecialForm("fn*")

Function NewLambda(objFunction)
	Dim objMal
	Set objMal = New MalType
	Set objMal.Value = New BuiltInFunction
	Set objMal.Value.Run = objFunction
	objMal.Type = TYPE_LAMBDA
	Set NewLambda = objMal
End Function

Class BuiltInFunction
	Public Run
End Class

Function NewSpecialForm(strValue)
	Set NewSpecialForm = New MalType
	NewSpecialForm.Value = strValue
	NewSpecialForm.Type = TYPE_SPECIAL
End Function

Function IsSpecialForm(objForm)
	IsSpecialForm = (objForm.Type = TYPE_SPECIAL)
End Function

Class SpecialForm
	Public Value
End Class

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
		' there's a bug that Item(0) maybe eval twice.
		If IsSpecialForm(objSymbol) Then
			'MsgBox TypeName(objCode.value)
			Select Case objSymbol.Value
				Case "def!"
					CheckArgNum objCode, 2
					CheckSymbol objCode.Value.Item(1)
					objEnv.Add objCode.Value.Item(1).Value, _
						Evaluate(objCode.Value.Item(2), objEnv)
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
				Case "do"
					Set Evaluate = EvaluateAST(objCode, objEnv)
					Set Evaluate = Evaluate.Value.Item(Evaluate.Value.Count - 1)
				Case "if"
					Dim objCondition
					'MsgBox 1
					Set objCondition = Evaluate(objCode.Value.Item(1), objEnv)
					'MsgBox 2
					If IsNil(objCondition) Or IsFalse(objCondition) Then
						Select Case objCode.Value.Count - 1
							Case 2
								Set Evaluate = New MalType
								Evaluate.Type = TYPE_NIL
								Evaluate.Value = Null
							Case 3
								Set Evaluate = Evaluate(objCode.Value.Item(3), objEnv)
							Case Else
								'TODO Err
						End Select
					Else
						If objCode.Value.Count - 1 = 2 Or objCode.Value.Count - 1 = 3 Then
							Set Evaluate = Evaluate(objCode.Value.Item(2), objEnv)
						Else
							'TODO err
						End If
					End If
				Case "fn*" 'lambda
					CheckArgNum objCode, 2
					Set Evaluate = New MalType
					Evaluate.Type = TYPE_LAMBDA
					Set Evaluate.Value = New Lambda
					'MsgBox 1
					Set Evaluate.Value.objEnv = New Environment
					Evaluate.Value.objEnv.SetSelf Evaluate.Value.objEnv
					Evaluate.Value.objEnv.SetOuter objEnv
					Set Evaluate.Value.objParameters = objCode.Value.Item(1)
					Set Evaluate.Value.objBody = objCode.Value.Item(2)
					'MsgBox 1
			End Select
		Else
			Set Evaluate = objSymbol.Value.Run(EvaluateAST(objCode, objEnv))
		End If
	Else
		Set Evaluate = EvaluateAST(objCode, objEnv)
	End If
End Function

Class Lambda
	Public objEnv
	Public objParameters
	Public objBody
	Public Function Run(objArgs)
		'MsgBox objArgs.type
		objEnv.Init objParameters, objArgs
		'para start from 0, args start from 1
		Set Run = Evaluate(objBody, objEnv)
	End Function
End Class

Function IsFalse(objMal)
	IsFalse = (objMal.Value = False)
End Function

Function IsNil(objMal)
	IsNil = (objMal.Type = TYPE_NIL)
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
			Set objResult = objEnv.Get(objCode.Value)
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