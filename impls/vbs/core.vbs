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

	Set varRet = Evaluate(objArgs.Item(2), objNewEnv)
	Set MLet = varRet
End Function
objNS.Add NewMalSym("let*"), NewVbsProc("MLet", True)

Function MDo(objArgs, objEnv)
	Dim varRet, i
	For i = 1 To objArgs.Count - 1
		Set varRet = Evaluate(objArgs.Item(i), objEnv)
	Next
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
	CheckListOrVec objArgs.Item(1)

	Set MCount = NewMalNum(objArgs.Item(1).Count)
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

REP "(def! not (fn* [bool] (if bool false true)))"
REP "(def! <= (fn* [a b] (not (> a b))))"
REP "(def! < (fn* [a b] (> b a)))"
REP "(def! >= (fn* [a b] (not (> b a))))"