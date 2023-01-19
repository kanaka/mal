Option Explicit

Include "Types.vbs"
Include "Reader.vbs"
Include "Printer.vbs"

Class Enviroment
	Private objDict
	Private objSelf

	Private Sub Class_Initialize
		Set objDict = CreateObject("Scripting.Dictionary")
	End Sub

	Public Function Add(objSymbol, objProcedure)
		objDict.Add objSymbol.Value, objProcedure
	End Function

	Public Property Set Self(objThis)
		Set objSelf = objThis
	End Property

	Public Function Find(varKey)
		Set Find = objSelf
	End Function

	Public Function [Get](objSymbol)
		If objDict.Exists(objSymbol.Value) Then
			Set [Get] = objDict.Item(objSymbol.Value)
		Else
			Err.Raise vbObjectError, _
				"Enviroment", "Symbol '" + PrintMalType(objSymbol, True) + "' not found."
		End If
	End Function
End Class

Dim objEnv
Set objEnv = New Enviroment
Set objEnv.Self = objEnv

Function Add(objArgs)
	CheckArgNum objArgs, 2
	Set Add = NewMalNum( _
		objArgs.Item(1).Value + objArgs.Item(2).Value)
End Function
objEnv.Add NewMalSym("+"), NewVbsProc("Add", False)

Function [Sub](objArgs)
	CheckArgNum objArgs, 2
	Set [Sub] = NewMalNum( _
		objArgs.Item(1).Value - objArgs.Item(2).Value)
End Function
objEnv.Add NewMalSym("-"), NewVbsProc("Sub", False)

Function Mul(objArgs)
	CheckArgNum objArgs, 2
	Set Mul = NewMalNum( _
		objArgs.Item(1).Value * objArgs.Item(2).Value)
End Function
objEnv.Add NewMalSym("*"), NewVbsProc("Mul", False)

Function Div(objArgs)
	CheckArgNum objArgs, 2
	Set Div = NewMalNum( _
		objArgs.Item(1).Value \ objArgs.Item(2).Value)
End Function
objEnv.Add NewMalSym("/"), NewVbsProc("Div", False)

Sub CheckArgNum(objArgs, lngArgNum)
	If objArgs.Count - 1 <> lngArgNum Then
		Err.Raise vbObjectError, _
			"CheckArgNum", "Wrong number of arguments."
	End IF
End Sub

Call REPL()
Sub REPL()
	Dim strCode, strResult
	While True
		WScript.StdOut.Write("user> ")

		On Error Resume Next
			strCode = WScript.StdIn.ReadLine()
			If Err.Number <> 0 Then WScript.Quit 0
		On Error Goto 0

		On Error Resume Next
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
				Set objCode.Item(i) = Evaluate(objCode.Item(i))
			Next
			Set varRet = objCode
		Case TYPES.HASHMAP
			For Each i In objCode.Keys()
				Set objCode.Item(i) = Evaluate(objCode.Item(i))
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