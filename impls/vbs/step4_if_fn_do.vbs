Option Explicit

Include "Types.vbs"
Include "Reader.vbs"
Include "Printer.vbs"
Include "Env.vbs"
Include "Core.vbs"

Dim objEnv
Set objEnv = objNS

Function MDef(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.SYMBOL
	Set varRet = Evaluate(objArgs.Item(2), objEnv)
	objEnv.Add objArgs.Item(1), varRet
	Set MDef = varRet
End Function
objEnv.Add NewMalSym("def!"), NewVbsProc("MDef", True)

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

	Set varRet = Evaluate(objArgs.Item(2), objNewEnv)
	Set MLet = varRet
End Function
objEnv.Add NewMalSym("let*"), NewVbsProc("MLet", True)

Function MDo(objArgs, objEnv)
	Dim varRet, i
	For i = 1 To objArgs.Count - 1
		Set varRet = Evaluate(objArgs.Item(i), objEnv)
	Next
	Set MDo = varRet
End Function
objEnv.Add NewMalSym("do"), NewVbsProc("MDo", True)

Function MIf(objArgs, objEnv)
	Dim varRet
	If objArgs.Count - 1 <> 3 And _
		objArgs.Count - 1 <> 2 Then
		Err.Raise vbObjectError, _
			"MIf", "Wrong number of arguments."
	End If

	If Evaluate(objArgs.Item(1), objEnv).Value Then
		Set varRet = Evaluate(objArgs.Item(2), objEnv)
	Else
		If objArgs.Count - 1 = 3 Then
			Set varRet = Evaluate(objArgs.Item(3), objEnv)
		Else
			Set varRet = NewMalNil()
		End If
	End If
	Set MIf = varRet
End Function
objEnv.Add NewMalSym("if"), NewVbsProc("MIf", True)

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
objEnv.Add NewMalSym("fn*"), NewVbsProc("MFn", True)

Call REPL()
Sub REPL()
	Dim strCode, strResult
	While True
		WScript.StdOut.Write("user> ")

		On Error Resume Next
			strCode = WScript.StdIn.ReadLine()
			If Err.Number <> 0 Then WScript.Quit 0
		On Error Goto 0

		'On Error Resume Next
			WScript.Echo REP(strCode)
			If Err.Number <> 0 Then
				WScript.StdErr.WriteLine Err.Source + ": " + Err.Description 
			End If
		On Error Goto 0
	Wend
End Sub

Function Read(strCode)
	Set Read = ReadString(strCode)
End Function

Function Evaluate(objCode, objEnv) ' Return Nothing / objCode
	If TypeName(objCode) = "Nothing" Then
		Set Evaluate = Nothing
		Exit Function
	End If
	Dim varRet
	If objCode.Type = TYPES.LIST Then
		If objCode.Count = 0 Then ' ()
			Set Evaluate = objCode
			Exit Function
		End If
		Set objCode.Item(0) = Evaluate(objCode.Item(0), objEnv)
		Set varRet = objCode.Item(0).Apply(objCode, objEnv)
	Else
		Set varRet = EvaluateAST(objCode, objEnv)
	End If

	Set Evaluate = varRet
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
			For i = 0 To objCode.Count() - 1
				Set objCode.Item(i) = Evaluate(objCode.Item(i), objEnv)
			Next
			Set varRet = objCode
		Case TYPES.HASHMAP
			For Each i In objCode.Keys()
				Set objCode.Item(i) = Evaluate(objCode.Item(i), objEnv)
			Next
			Set varRet = objCode
		Case Else
			Set varRet = objCode
	End Select
	Set EvaluateAST = varRet
End Function

Function EvaluateRest(objCode, objEnv)
	Dim varRet, i
	Select Case objCode.Type
		Case TYPES.LIST
			For i = 1 To objCode.Count() - 1
				Set objCode.Item(i) = Evaluate(objCode.Item(i), objEnv)
			Next
			Set varRet = objCode
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