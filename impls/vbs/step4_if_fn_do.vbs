'TODO 字符串有问题

Option Explicit
Dim DEPTH
DEPTH = 0
Dim CALLFROM
CALLFROM = ""
Include "Core.vbs"
Include "Reader.vbs"
Include "Printer.vbs"
Include "Env.vbs"

Dim objRootEnv
Set objRootEnv = New Environment
objRootEnv.SetSelf objRootEnv
objRootEnv.SetOuter Nothing
Dim arrKeys, i
arrKeys = objCoreNS.Keys
For i = 0 To UBound(arrKeys)
	objRootEnv.Add arrKeys(i), NewLambda(objCoreNS.Item(arrKeys(i)))
Next
objRootEnv.Add "def!", NewSpecialForm("def!")
objRootEnv.Add "let*", NewSpecialForm("let*")
objRootEnv.Add "do", NewSpecialForm("do")
objRootEnv.Add "if", NewSpecialForm("if")
objRootEnv.Add "fn*", NewSpecialForm("fn*")
REP "(def! not (fn* (a) (if a false true)))"

Function NewLambda(objFunction)
	Dim objMal
	Set objMal = New MalType
	Set objMal.Value = New BuiltInFunction
	Set objMal.Value.Run = objFunction
	objMal.Type = TYPE_LAMBDA
	Set NewLambda = objMal
End Function

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
	If objArgs.Value.Count - 1 <> lngExpect Then
		boolError = True
		strError = "wrong number of arguments"
		Call REPL()
	End If
End Sub

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
	DEPTH = DEPTH + 1
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
		'wsh.echo space(DEPTH*4)&"CHECK FIRST"
		Set objSymbol = Evaluate(objCode.Value.Item(0), objEnv)
		'wsh.echo space(DEPTH*4)&"CHECK FIRST FINISH"
		'MsgBox objSymbol.type
		If IsSpecialForm(objSymbol) Then
			'wsh.echo space(DEPTH*4)&"EVAL SPECIAL"
			'MsgBox TypeName(objCode.value)
			Select Case objSymbol.Value
				Case "def!"
					'MsgBox "我在def"
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
					'MsgBox IsNil(objCondition)
					'MsgBox IsFalse(objCondition)
					If IsNil(objCondition) Or IsFalse(objCondition) Then
						'MsgBox 1
						Select Case objCode.Value.Count - 1
							Case 2
								Set Evaluate = New MalType
								Evaluate.Type = TYPE_NIL
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
					'Set Evaluate.Value.objEnv = New Environment
					'Evaluate.Value.objEnv.SetSelf Evaluate.Value.objEnv
					'Evaluate.Value.objEnv.SetOuter objEnv
					Set Evaluate.Value.objParameters = objCode.Value.Item(1)
					Set Evaluate.Value.objBody = objCode.Value.Item(2)
					'MsgBox 1
			End Select
			'wsh.echo space(DEPTH*4)&"EVAL SPECIAL FINISH"
		Else
			'wsh.echo space(DEPTH*4)&"EVAL NORMAL"
			'MsgBox 2
			'objSymbol.Value.SetEnv objEnv
			'wsh.echo space(DEPTH*4)&"objcode","type",objCode.Type
			'If objCode.Type = 7 Then wsh.echo space(DEPTH*4)&objCode.value
			'If objCode.Type = 8 Then wsh.echo space(DEPTH*4)&objCode.value
			'If objCode.Type = 0 Then wsh.echo space(DEPTH*4)&PrintMalType(objCode,True)

			'这里有大问题
			If objSymbol.Value.IsBuiltIn Then
				Set objSymbol.Value.objEnv = New Environment
				objSymbol.Value.objEnv.SetSelf objSymbol.Value.objEnv
				objSymbol.Value.objEnv.SetOuter objEnv
				Set Evaluate = objSymbol.Value.Run(objCode)

			Else
				Set Evaluate = objSymbol.Value.Run(EvaluateAST(objCode, objEnv))
			End If
			'wsh.echo space(DEPTH*4)&"evaluate","type",Evaluate.Type
			'If Evaluate.Type = 7 Then wsh.echo space(DEPTH*4)&Evaluate.value
			'If Evaluate.Type = 8 Then wsh.echo space(DEPTH*4)&Evaluate.value
			'If Evaluate.Type = 0 Then wsh.echo space(DEPTH*4)&PrintMalType(Evaluate,True)
			'Set Evaluate = Evaluate(objCode, objEnv)
			'MsgBox Evaluate.type
			'MsgBox objEnv.Get("N").value
			'MsgBox 3
			'wsh.echo space(DEPTH*4)&"EVAL NORMAL FINISH"
		End If
	Else
		'wsh.echo space(DEPTH*4)&"objcode","type",objCode.Type
		'If objCode.Type = 7 Then wsh.echo space(DEPTH*4)&objCode.value
		'If objCode.Type = 8 Then wsh.echo space(DEPTH*4)&objCode.value
		'If objCode.Type = 0 Then wsh.echo space(DEPTH*4)&PrintMalType(objCode,True)
		Set Evaluate = EvaluateAST(objCode, objEnv)
		'wsh.echo space(DEPTH*4)&"evaluate","type",Evaluate.Type
		'If Evaluate.Type = 7 Then wsh.echo space(DEPTH*4)&Evaluate.value
		'If Evaluate.Type = 8 Then wsh.echo space(DEPTH*4)&Evaluate.value
		'If Evaluate.Type = 0 Then wsh.echo space(DEPTH*4)&PrintMalType(Evaluate,True)
		'wsh.echo ""
	End If
	'wsh.echo space(DEPTH*4)&"RETURN"
	DEPTH = DEPTH - 1
End Function

Class BuiltInFunction
	Public IsBuiltIn
	Public Sub Class_Initialize
		IsBuiltIn = False
	End Sub
	Public Run
	Public Sub SetEnv(z)
	End Sub
End Class

Class Lambda
	Public objParameters
	Public objBody
	Public objEnv
	Public IsBuiltIn
	Public Sub Class_Initialize
		IsBuiltIn = True
	End Sub
	Public Function SetEnv(oInv)
		Set objEnv=oInv
	End Function

	Public Function Run(objArgs)
		Dim objNewEnv
		Set objNewEnv = New Environment
		objNewEnv.SetSelf objNewEnv
		objNewEnv.SetOuter objEnv
		'MsgBox objArgs.type
		objNewEnv.Init objParameters, objArgs
		'para start from 0, args start from 1
		'MsgBox objNewEnv.Get("N").value
		'wsh.echo space(DEPTH*4)&"RUN "& PrintMalType(objBody,True)
		Set Run = Evaluate(objBody, objNewEnv)
		'wsh.echo space(DEPTH*4)&"RUN FINISH"
		'MsgBox Run.type
		'MsgBox Run.value
	End Function
End Class

Function IsZero(objMal)
	IsZero = (objMal.Type = TYPE_NUMBER And objMal.Value = 0)
	'MsgBox IsZero
End Function

Function IsFalse(objMal)
	IsFalse = (objMal.Type = TYPE_BOOLEAN)
	If Not IsFalse Then Exit Function
	IsFalse = IsFalse And (objMal.Value = False)
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
			Set objResult = New MalType
			Set objResult.Value = CreateObject("System.Collections.ArrayList")
			objResult.Type = TYPE_LIST
			For i = 0 To objCode.Value.Count - 1
				objResult.Value.Add Evaluate(objCode.Value.Item(i), objEnv)
			Next
		Case TYPE_VECTOR
			Set objResult = New MalType
			Set objResult.Value = CreateObject("System.Collections.ArrayList")
			objResult.Type = TYPE_VECTOR
			For i = 0 To objCode.Value.Count - 1
				objResult.Value.Add Evaluate(objCode.Value.Item(i), objEnv)
			Next
		Case TYPE_HASHMAP
			Set objResult = New MalType
			Set objResult.Value = CreateObject("Scripting.Dictionary")
			objResult.Type = TYPE_HASHMAP
			Dim key
			For Each key In objCode.Value.Keys
				objResult.Value.Add Evaluate(key, objEnv), Evaluate(objCode.Value.Item(key), objEnv)
			Next
		Case Else
			Set objResult = objCode
	End Select
	Set EvaluateAST = objResult
End Function

Function Print(objCode)
	Print = PrintMalType(objCode, True)
End Function

Function REP(strCode)
	REP = Print(Evaluate(Read(strCode), objRootEnv))
End Function

Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub
