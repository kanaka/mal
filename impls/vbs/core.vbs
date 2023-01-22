Option Explicit

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

Function IsListOrVec(objMal)
	IsListOrVec = _
		objMal.Type = TYPES.LIST Or _
		objMal.Type = TYPES.VECTOR
End Function

Sub CheckListOrVec(objMal)
	If Not IsListOrVec(objMal) Then
		Err.Raise vbObjectError, _
			"CheckListOrVec", _
			"Wrong argument type, need a list or a vector."
	End If
End Sub

Dim objNS
Set objNS = NewEnv(Nothing)

Function MAdd(objArgs)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MAdd = NewMalNum( _
		objArgs.Item(1).Value + objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("+"), NewVbsProc("MAdd", False)

Function MSub(objArgs)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MSub = NewMalNum( _
		objArgs.Item(1).Value - objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("-"), NewVbsProc("MSub", False)

Function MMul(objArgs)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MMul = NewMalNum( _
		objArgs.Item(1).Value * objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("*"), NewVbsProc("MMul", False)

Function MDiv(objArgs)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MDiv = NewMalNum( _
		objArgs.Item(1).Value \ objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("/"), NewVbsProc("MDiv", False)

Function MList(objArgs)
	Dim varRet
	Set varRet = NewMalList(Array())
	Dim i
	For i = 1 To objArgs.Count - 1
		varRet.Add objArgs.Item(i)
	Next
	Set MList = varRet
End Function
objNS.Add NewMalSym("list"), NewVbsProc("MList", False)

Function MIsList(objArgs)
	CheckArgNum objArgs, 1

	Set MIsList = NewMalBool(objArgs.Item(1).Type = TYPES.LIST)
End Function
objNS.Add NewMalSym("list?"), NewVbsProc("MIsList", False)

Function MIsEmpty(objArgs)
	CheckArgNum objArgs, 1
	CheckListOrVec objArgs.Item(1)

	Set MIsEmpty = NewMalBool(objArgs.Item(1).Count = 0)
End Function
objNS.Add NewMalSym("empty?"), NewVbsProc("MIsEmpty", False)

Function MCount(objArgs)
	CheckArgNum objArgs, 1
	If objArgs.Item(1).Type = TYPES.NIL Then
		Set MCount = NewMalNum(0)
	Else
		CheckListOrVec objArgs.Item(1)
		Set MCount = NewMalNum(objArgs.Item(1).Count)
	End If
End Function
objNS.Add NewMalSym("count"), NewVbsProc("MCount", False)

Function MEqual(objArgs)
	Dim varRet
	CheckArgNum objArgs, 2

	Dim boolResult, i
	If IsListOrVec(objArgs.Item(1)) And _
		IsListOrVec(objArgs.Item(2)) Then
		If objArgs.Item(1).Count <> objArgs.Item(2).Count Then
			Set varRet = NewMalBool(False)
		Else
			boolResult = True
			For i = 0 To objArgs.Item(1).Count - 1
				boolResult = boolResult And _
					MEqual(NewMalList(Array(Nothing, _
					objArgs.Item(1).Item(i), _
					objArgs.Item(2).Item(i)))).Value
			Next
			Set varRet = NewMalBool(boolResult)	
		End If
	Else
		If objArgs.Item(1).Type <> objArgs.Item(2).Type Then
			Set varRet = NewMalBool(False)
		Else
			Select Case objArgs.Item(1).Type
				Case TYPES.HASHMAP
					Err.Raise vbObjectError, _
						"MEqual", "Not implement yet~"
				Case Else
					Set varRet = NewMalBool( _
						objArgs.Item(1).Value = objArgs.Item(2).Value)
			End Select
		End If
	End If

	Set MEqual = varRet
End Function
objNS.Add NewMalSym("="), NewVbsProc("MEqual", False)

Function MGreater(objArgs)
	Dim varRet
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set varRet = NewMalBool( _
		objArgs.Item(1).Value > objArgs.Item(2).Value)
	Set MGreater = varRet
End Function
objNS.Add NewMalSym(">"), NewVbsProc("MGreater", False)

Function MPrStr(objArgs)
	Dim varRet
	Dim strRet
	strRet = ""
	Dim i
	If objArgs.Count - 1 >= 1 Then
		strRet = PrintMalType(objArgs.Item(1), True)
	End If
	For i = 2 To objArgs.Count - 1
		strRet = strRet + " " + _
			PrintMalType(objArgs.Item(i), True)
	Next
	Set varRet = NewMalStr(strRet)
	Set MPrStr = varRet
End Function
objNS.Add NewMalSym("pr-str"), NewVbsProc("MPrStr", False)

Function MStr(objArgs)
	Dim varRet
	Dim strRet
	strRet = ""
	Dim i
	For i = 1 To objArgs.Count - 1
		strRet = strRet + _
			PrintMalType(objArgs.Item(i), False)
	Next
	Set varRet = NewMalStr(strRet)
	Set MStr = varRet
End Function
objNS.Add NewMalSym("str"), NewVbsProc("MStr", False)

Function MPrn(objArgs)
	Dim varRet
	Dim objStr
	Set objStr = MPrStr(objArgs)
	WScript.StdOut.WriteLine objStr.Value
	Set varRet = NewMalNil()
	Set MPrn = varRet
End Function
objNS.Add NewMalSym("prn"), NewVbsProc("MPrn", False)

Function MPrintln(objArgs)
	Dim varRet
	Dim strRes
	strRes = ""
	Dim i
	If objArgs.Count - 1 >= 1 Then
		strRes = PrintMalType(objArgs.Item(1), False)
	End If
	For i = 2 To objArgs.Count - 1
		strRes = strRes + " " + _
			PrintMalType(objArgs.Item(i), False)
	Next
	WScript.StdOut.WriteLine strRes
	Set varRet = NewMalNil()
	Set MPrintln = varRet
End Function
objNS.Add NewMalSym("println"), NewVbsProc("MPrintln", False)

Sub InitBuiltIn()
	REP "(def! not (fn* [bool] (if bool false true)))"
	REP "(def! <= (fn* [a b] (not (> a b))))"
	REP "(def! < (fn* [a b] (> b a)))"
	REP "(def! >= (fn* [a b] (not (> b a))))"
	REP "(def! load-file (fn* (f) (eval (read-string (str ""(do "" (slurp f) ""\nnil)"")))))"
End Sub

Function MReadStr(objArgs)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.STRING

	Set varRes = ReadString(objArgs.Item(1).Value)
	Set MReadStr = varRes
End Function
objNS.Add NewMalSym("read-string"), NewVbsProc("MReadStr", False)

Function MSlurp(objArgs)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.STRING

	Dim strRes
	With CreateObject("Scripting.FileSystemObject")
		strRes = .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & objArgs.Item(1).Value).ReadAll
	End With

	Set varRes = NewMalStr(strRes)
	Set MSlurp = varRes
End Function
objNS.Add NewMalSym("slurp"), NewVbsProc("MSlurp", False)

Function MAtom(objArgs)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = NewMalAtom(objArgs.Item(1))
	Set MAtom = varRes
End Function
objNS.Add NewMalSym("atom"), NewVbsProc("MAtom", False)

Function MIsAtom(objArgs)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = NewMalBool(objArgs.Item(1).Type = TYPES.ATOM)
	Set MIsAtom = varRes
End Function
objNS.Add NewMalSym("atom?"), NewVbsProc("MIsAtom", False)

Function MDeref(objArgs)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.ATOM

	Set varRes = objArgs.Item(1).Value
	Set MDeref = varRes
End Function
objNS.Add NewMalSym("deref"), NewVbsProc("MDeref", False)

Function MReset(objArgs)
	Dim varRes
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.ATOM

	objArgs.Item(1).Reset objArgs.Item(2)
	Set varRes = objArgs.Item(2)
	Set MReset = varRes
End Function
objNS.Add NewMalSym("reset!"), NewVbsProc("MReset", False)

Function MSwap(objArgs, objEnv)
	Dim varRes
	If objArgs.Count - 1 < 2 Then
		Err.Raise vbObjectError, _
			"MSwap", "Need more arguments."
	End If

	Dim objAtom
	Set objAtom = Evaluate(objArgs.Item(1), objEnv)
	CheckType objAtom, TYPES.ATOM
	
	Dim objFn
	Set objFn = Evaluate(objArgs.Item(2), objEnv)
	CheckType objFn, TYPES.PROCEDURE

	Dim objProc
	Set objProc = NewMalList(Array(objFn))
	objProc.Add objAtom.Value
	Dim i
	For i = 3 To objArgs.Count - 1
		objProc.Add Evaluate(objArgs.Item(i), objEnv)
	Next

	objAtom.Reset Evaluate(objProc, objEnv)
	Set varRes = objAtom.Value
	Set MSwap = varRes
End Function
objNS.Add NewMalSym("swap!"), NewVbsProc("MSwap", True)
