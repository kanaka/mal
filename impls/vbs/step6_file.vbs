Option Explicit

Include "IO.vbs"
Include "Types.vbs"
Include "Reader.vbs"
Include "Printer.vbs"
Include "Env.vbs"
Include "Core.vbs"

Class TailCall
	Public objMalType
	Public objEnv
End Class

Function EvalLater(objMal, objEnv)
	Dim varRes
	Set varRes = New TailCall
	Set varRes.objMalType = objMal
	Set varRes.objEnv = objEnv
	Set EvalLater = varRes
End Function

Function MDef(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.SYMBOL
	Set varRet = Evaluate(objArgs.Item(2), objEnv)
	objEnv.Add objArgs.Item(1), varRet
	Set MDef = varRet
End Function
objNS.Add NewMalSym("def!"), NewVbsProc("MDef", True)

Function MLet(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2

	Dim objBinds
	Set objBinds = objArgs.Item(1)
	CheckListOrVec objBinds
	
	If objBinds.Count Mod 2 <> 0 Then
		Err.Raise vbObjectError, _
			"MLet", "Wrong argument count."
	End If

	Dim objNewEnv
	Set objNewEnv = NewEnv(objEnv)
	Dim i, objSym
	For i = 0 To objBinds.Count - 1 Step 2
		Set objSym = objBinds.Item(i)
		CheckType objSym, TYPES.SYMBOL
		objNewEnv.Add objSym, Evaluate(objBinds.Item(i + 1), objNewEnv)
	Next

	Set varRet = EvalLater(objArgs.Item(2), objNewEnv)
	Set MLet = varRet
End Function
objNS.Add NewMalSym("let*"), NewVbsProc("MLet", True)

Function MDo(objArgs, objEnv)
	Dim varRet, i
	If objArgs.Count - 1 < 1 Then
		Err.Raise vbObjectError, _
			"MDo", "Need more arguments."
	End If
	For i = 1 To objArgs.Count - 2
		Call Evaluate(objArgs.Item(i), objEnv)
	Next
	Set varRet = EvalLater( _
		objArgs.Item(objArgs.Count - 1), _
		objEnv)
	Set MDo = varRet
End Function
objNS.Add NewMalSym("do"), NewVbsProc("MDo", True)

Function MIf(objArgs, objEnv)
	Dim varRet
	If objArgs.Count - 1 <> 3 And _
		objArgs.Count - 1 <> 2 Then
		Err.Raise vbObjectError, _
			"MIf", "Wrong number of arguments."
	End If

	Dim objCond
	Set objCond = Evaluate(objArgs.Item(1), objEnv)
	Dim boolCond
	If objCond.Type = TYPES.BOOLEAN Then
		boolCond = objCond.Value
	Else
		boolCond = True
	End If
	boolCond = (boolCond And objCond.Type <> TYPES.NIL)
	If boolCond Then
		Set varRet = EvalLater(objArgs.Item(2), objEnv)
	Else
		If objArgs.Count - 1 = 3 Then
			Set varRet = EvalLater(objArgs.Item(3), objEnv)
		Else
			Set varRet = NewMalNil()
		End If
	End If
	Set MIf = varRet
End Function
objNS.Add NewMalSym("if"), NewVbsProc("MIf", True)

Function MFn(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2

	Dim objParams, objCode
	Set objParams = objArgs.Item(1)
	CheckListOrVec objParams
	Set objCode = objArgs.Item(2)
	
	Dim i
	For i = 0 To objParams.Count - 1
		CheckType objParams.Item(i), TYPES.SYMBOL
	Next
	Set varRet = NewMalProc(objParams, objCode, objEnv)
	Set MFn = varRet
End Function
objNS.Add NewMalSym("fn*"), NewVbsProc("MFn", True)

Function MEval(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = Evaluate(objArgs.Item(1), objEnv)
	Set varRes = EvalLater(varRes, objNS)
	Set MEval = varRes
End Function
objNS.Add NewMalSym("eval"), NewVbsProc("MEval", True)

Call InitBuiltIn()

Call InitArgs()
Sub InitArgs()
	Dim objArgs
	Set objArgs = NewMalList(Array())

	Dim i
	For i = 1 To WScript.Arguments.Count - 1
		objArgs.Add NewMalStr(WScript.Arguments.Item(i))
	Next
	
	objNS.Add NewMalSym("*ARGV*"), objArgs
	
	If WScript.Arguments.Count > 0 Then
		REP "(load-file """ + WScript.Arguments.Item(0) + """)"
		WScript.Quit 0
	End If
End Sub

Call REPL()
Sub REPL()
	Dim strCode
	While True
		IO.Write "user> "

		On Error Resume Next
			strCode = IO.ReadLine
			If Err.Number <> 0 Then WScript.Quit 0
		On Error Goto 0

		Dim strRes
		On Error Resume Next
			strRes = REP(strCode)
			If Err.Number <> 0 Then
				IO.WriteErrLine "Exception: " + Err.Description
			Else
				If strRes <> "" Then
					IO.WriteLine strRes
				End If
			End If
		On Error Goto 0
	Wend
End Sub

Function Read(strCode)
	Set Read = ReadString(strCode)
End Function

Function Evaluate(ByVal objCode, ByVal objEnv)
	While True
		If TypeName(objCode) = "Nothing" Then
			Set Evaluate = Nothing
			Exit Function
		End If
		
		Dim varRet, objFirst
		If objCode.Type = TYPES.LIST Then
			If objCode.Count = 0 Then ' ()
				Set Evaluate = objCode
				Exit Function
			End If
			Set objFirst = Evaluate(objCode.Item(0), objEnv)
			Set varRet = objFirst.Apply(objCode, objEnv)
		Else
			Set varRet = EvaluateAST(objCode, objEnv)
		End If
		
		If TypeName(varRet) = "TailCall" Then
			' NOTICE: If not specify 'ByVal', 
			' Change of arguments will influence
			' the caller's variable!
			Set objCode = varRet.objMalType
			Set objEnv = varRet.objEnv
		Else
			Set Evaluate = varRet
			Exit Function
		End If
	Wend
End Function


Function EvaluateAST(objCode, objEnv)
	Dim varRet, i
	Select Case objCode.Type
		Case TYPES.SYMBOL
			Set varRet = objEnv.Get(objCode)
		Case TYPES.LIST
			Err.Raise vbObjectError, _
				"EvaluateAST", "Unexpect type."
		Case TYPES.VECTOR
			Set varRet = NewMalVec(Array())
			For i = 0 To objCode.Count() - 1
				varRet.Add Evaluate(objCode.Item(i), objEnv)
			Next
		Case TYPES.HASHMAP
			Set varRet = NewMalMap(Array(), Array())
			For Each i In objCode.Keys()
				varRet.Add i, Evaluate(objCode.Item(i), objEnv)
			Next
		Case Else
			Set varRet = objCode
	End Select
	Set EvaluateAST = varRet
End Function

Function EvaluateRest(objCode, objEnv)
	Dim varRet, i
	Select Case objCode.Type
		Case TYPES.LIST
			Set varRet = NewMalList(Array(NewMalNil()))
			For i = 1 To objCode.Count() - 1
				varRet.Add Evaluate(objCode.Item(i), objEnv)
			Next
		Case Else
			Err.Raise vbObjectError, _
				"EvaluateRest", "Unexpected type."
	End Select
	Set EvaluateRest = varRet
End Function

Function Print(objCode)
	Print = PrintMalType(objCode, True)
End Function

Function REP(strCode)
	REP = Print(Evaluate(Read(strCode), objNS))
End Function

Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub