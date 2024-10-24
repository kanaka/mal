Option Explicit

Include "IO.vbs"
Include "Types.vbs"
Include "Reader.vbs"
Include "Printer.vbs"

	Function EnvGet(objDict, objSymbol)
		If objDict.Exists(objSymbol) Then
			Set EnvGet = objDict.Item(objSymbol)
		Else
			Err.Raise vbObjectError, _
				"Enviroment", "Symbol '" + objSymbol + "' not found."
		End If
	End Function

Dim objEnv
Set objEnv = CreateObject("Scripting.Dictionary")

Function MAdd(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MAdd = NewMalNum( _
		objArgs.Item(1).Value + objArgs.Item(2).Value)
End Function
objEnv.Add "+", NewVbsProc("MAdd", False)

Function MSub(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MSub = NewMalNum( _
		objArgs.Item(1).Value - objArgs.Item(2).Value)
End Function
objEnv.Add "-", NewVbsProc("MSub", False)

Function MMul(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MMul = NewMalNum( _
		objArgs.Item(1).Value * objArgs.Item(2).Value)
End Function
objEnv.Add "*", NewVbsProc("MMul", False)

Function MDiv(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MDiv = NewMalNum( _
		objArgs.Item(1).Value \ objArgs.Item(2).Value)
End Function
objEnv.Add "/", NewVbsProc("MDiv", False)

Sub CheckArgNum(objArgs, lngArgNum)
	If objArgs.Count - 1 <> lngArgNum Then
		Err.Raise vbObjectError, _
			"CheckArgNum", "Wrong number of arguments."
	End IF
End Sub

Sub CheckType(objMal, varType)
	If objMal.Type <> varType Then
		Err.Raise vbObjectError, _
			"CheckType", "Wrong argument type."
	End IF
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

Function Evaluate(objCode, objEnv)
	If TypeName(objCode) = "Nothing" Then
		Set Evaluate = Nothing
		Exit Function
	End If

	' DebugEval objCode, objEnv

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

	Set Evaluate = varRet
End Function


Function EvaluateAST(objCode, objEnv)
	Dim varRet, i
	Select Case objCode.Type
		Case TYPES.SYMBOL
			Set varRet = EnvGet(objEnv, objCode.Value)
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
