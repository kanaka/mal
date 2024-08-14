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

Function MAdd(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MAdd = NewMalNum( _
		objArgs.Item(1).Value + objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("+"), NewVbsProc("MAdd", False)

Function MSub(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MSub = NewMalNum( _
		objArgs.Item(1).Value - objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("-"), NewVbsProc("MSub", False)

Function MMul(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MMul = NewMalNum( _
		objArgs.Item(1).Value * objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("*"), NewVbsProc("MMul", False)

Function MDiv(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set MDiv = NewMalNum( _
		objArgs.Item(1).Value \ objArgs.Item(2).Value)
End Function
objNS.Add NewMalSym("/"), NewVbsProc("MDiv", False)

Function MList(objArgs, objEnv)
	Dim varRet
	Set varRet = NewMalList(Array())
	Dim i
	For i = 1 To objArgs.Count - 1
		varRet.Add objArgs.Item(i)
	Next
	Set MList = varRet
End Function
objNS.Add NewMalSym("list"), NewVbsProc("MList", False)

Function MIsList(objArgs, objEnv)
	CheckArgNum objArgs, 1

	Set MIsList = NewMalBool(objArgs.Item(1).Type = TYPES.LIST)
End Function
objNS.Add NewMalSym("list?"), NewVbsProc("MIsList", False)

Function MIsEmpty(objArgs, objEnv)
	CheckArgNum objArgs, 1
	CheckListOrVec objArgs.Item(1)

	Set MIsEmpty = NewMalBool(objArgs.Item(1).Count = 0)
End Function
objNS.Add NewMalSym("empty?"), NewVbsProc("MIsEmpty", False)

Function MCount(objArgs, objEnv)
	CheckArgNum objArgs, 1
	If objArgs.Item(1).Type = TYPES.NIL Then
		Set MCount = NewMalNum(0)
	Else
		CheckListOrVec objArgs.Item(1)
		Set MCount = NewMalNum(objArgs.Item(1).Count)
	End If
End Function
objNS.Add NewMalSym("count"), NewVbsProc("MCount", False)

Function MEqual(objArgs, objEnv)
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
					objArgs.Item(2).Item(i))), objEnv).Value
			Next
			Set varRet = NewMalBool(boolResult)	
		End If
	Else
		If objArgs.Item(1).Type <> objArgs.Item(2).Type Then
			Set varRet = NewMalBool(False)
		Else
			Select Case objArgs.Item(1).Type
				Case TYPES.HASHMAP
					'Err.Raise vbObjectError, _
					'	"MEqual", "Not implement yet~"
					If UBound(objArgs.Item(1).Keys) <> UBound(objArgs.Item(2).Keys) Then
						Set varRet = NewMalBool(False)
						Set MEqual = varRet
						Exit Function
					End If
					
					boolResult = True
					For Each i In objArgs.Item(1).Keys
						If Not objArgs.Item(2).Exists(i) Then
							Set varRet = NewMalBool(False)
							Set MEqual = varRet
							Exit Function
						End If

						boolResult = boolResult And _
							MEqual(NewMalList(Array(Nothing, objArgs.Item(1).Item(i), objArgs.Item(2).Item(i))), objEnv).Value
					Next
					Set varRet = NewMalBool(boolResult)	
					
				Case Else
					Set varRet = NewMalBool( _
						objArgs.Item(1).Value = objArgs.Item(2).Value)
			End Select
		End If
	End If

	Set MEqual = varRet
End Function
objNS.Add NewMalSym("="), NewVbsProc("MEqual", False)

Function MGreater(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.NUMBER
	CheckType objArgs.Item(2), TYPES.NUMBER
	Set varRet = NewMalBool( _
		objArgs.Item(1).Value > objArgs.Item(2).Value)
	Set MGreater = varRet
End Function
objNS.Add NewMalSym(">"), NewVbsProc("MGreater", False)

Function MPrStr(objArgs, objEnv)
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

Function MStr(objArgs, objEnv)
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

Function MPrn(objArgs, objEnv)
	Dim varRet
	Dim objStr
	Set objStr = MPrStr(objArgs, objEnv)
	IO.WriteLine objStr.Value
	Set varRet = NewMalNil()
	Set MPrn = varRet
End Function
objNS.Add NewMalSym("prn"), NewVbsProc("MPrn", False)

Function MPrintln(objArgs, objEnv)
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
	IO.WriteLine strRes
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
	REP "(def! cons (fn* [a b] (concat (list a) b)))"
	REP "(def! nil? (fn* [x] (= x nil)))"
	REP "(def! true? (fn* [x] (= x true)))"
	REP "(def! false? (fn* [x] (= x false)))"
	REP "(def! vector (fn* [& args] (vec args)))"
	REP "(def! vals (fn* [hmap] (map (fn* [key] (get hmap key)) (keys hmap))))"
	REP "(def! *host-language* ""VBScript"")"
End Sub

Function MReadStr(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.STRING

	Set varRes = ReadString(objArgs.Item(1).Value)
	If TypeName(varRes) = "Nothing" Then
		Set varRes = NewMalNil()
	End If
	Set MReadStr = varRes
End Function
objNS.Add NewMalSym("read-string"), NewVbsProc("MReadStr", False)

Function MSlurp(objArgs, objEnv)
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

Function MAtom(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = NewMalAtom(objArgs.Item(1))
	Set MAtom = varRes
End Function
objNS.Add NewMalSym("atom"), NewVbsProc("MAtom", False)

Function MIsAtom(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = NewMalBool(objArgs.Item(1).Type = TYPES.ATOM)
	Set MIsAtom = varRes
End Function
objNS.Add NewMalSym("atom?"), NewVbsProc("MIsAtom", False)

Function MDeref(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.ATOM

	Set varRes = objArgs.Item(1).Value
	Set MDeref = varRes
End Function
objNS.Add NewMalSym("deref"), NewVbsProc("MDeref", False)

Function MReset(objArgs, objEnv)
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

	Dim objAtom, objFn
	Set objAtom = objArgs.Item(1)
	CheckType objAtom, TYPES.ATOM
	Set objFn = objArgs.Item(2)
	CheckType objFn, TYPES.PROCEDURE

	Dim objProg
	Set objProg = NewMalList(Array(objFn))
	objProg.Add objAtom.Value
	Dim i
	For i = 3 To objArgs.Count - 1
		objProg.Add objArgs.Item(i)
	Next

	objAtom.Reset objFn.ApplyWithoutEval(objProg, objEnv)
	Set varRes = objAtom.Value
	Set MSwap = varRes
End Function
objNS.Add NewMalSym("swap!"), NewVbsProc("MSwap", False)

Function MConcat(objArgs, objEnv)
	Dim varRes
	Dim i, j
	Set varRes = NewMalList(Array())
	For i = 1 To objArgs.Count - 1
		If Not IsListOrVec(objArgs.Item(i)) Then
			Err.Raise vbObjectError, _
				"MConcat", "Invaild argument(s)."
		End If
		
		For j = 0 To objArgs.Item(i).Count - 1
			varRes.Add objArgs.Item(i).Item(j)
		Next
	Next
	Set MConcat = varRes
End Function
objNS.Add NewMalSym("concat"), NewVbsProc("MConcat", False)

Function MVec(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckListOrVec objArgs.Item(1)
	Set varRes = NewMalVec(Array())
	Dim i
	For i = 0 To objArgs.Item(1).Count - 1
		varRes.Add objArgs.Item(1).Item(i)
	Next
	Set MVec = varRes
End Function
objNS.Add NewMalSym("vec"), NewVbsProc("MVec", False)

Function MNth(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 2
	CheckListOrVec objArgs.Item(1)
	CheckType objArgs.Item(2), TYPES.NUMBER

	If objArgs.Item(2).Value < objArgs.Item(1).Count Then
		Set varRes = objArgs.Item(1).Item(objArgs.Item(2).Value)
	Else
		Err.Raise vbObjectError, _
			"MNth", "Index out of bounds."
	End If

	Set MNth = varRes
End Function
objNS.Add NewMalSym("nth"), NewVbsProc("MNth", False)

Function MFirst(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	
	If objArgs.Item(1).Type = TYPES.NIL Then
		Set varRes = NewMalNil()
		Set MFirst = varRes
		Exit Function
	End If

	CheckListOrVec objArgs.Item(1)

	If objArgs.Item(1).Count < 1 Then
		Set varRes = NewMalNil()
	Else
		Set varRes = objArgs.Item(1).Item(0)
	End If

	Set MFirst = varRes
End Function
objNS.Add NewMalSym("first"), NewVbsProc("MFirst", False)

Function MRest(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	
	If objArgs.Item(1).Type = TYPES.NIL Then
		Set varRes = NewMalList(Array())
		Set MRest = varRes
		Exit Function
	End If

	Dim objList
	Set objList = objArgs.Item(1)
	CheckListOrVec objList

	Set varRes = NewMalList(Array())
	Dim i
	For i = 1 To objList.Count - 1
		varRes.Add objList.Item(i)
	Next
	
	Set MRest = varRes
End Function
objNS.Add NewMalSym("rest"), NewVbsProc("MRest", False)

Sub InitMacro()
	REP "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw ""odd number of forms to cond"")) (cons'cond (rest (rest xs)))))))"
	'REP "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"
	REP "(def! *gensym-counter* (atom 0))"
	REP "(def! gensym (fn* [] (symbol (str ""G__"" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"
	REP "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))"
End Sub

Class MalException
	Private objDict
	Private Sub Class_Initialize
		Set objDict = CreateObject("Scripting.Dictionary")
	End Sub

	Public Sub Add(varKey, varValue)
		objDict.Add varKey, varValue
	End Sub

	Public Function Item(varKey)
		Set Item = objDict.Item(varKey)
	End Function

	Public Sub Remove(varKey)
		objDict.Remove varKey
	End Sub
End Class

Dim objExceptions
Set objExceptions = New MalException

Function MThrow(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Dim strRnd
	strRnd = CStr(Rnd())
	objExceptions.Add strRnd, objArgs.Item(1)
	Err.Raise vbObjectError, _
		"MThrow", strRnd
End Function
objNS.Add NewMalSym("throw"), NewVbsProc("MThrow", False)

Function MApply(objArgs, objEnv)
	Dim varRes
	If objArgs.Count - 1 < 2 Then
		Err.Raise vbObjectError, _
			"MApply", "Need more arguments."
	End If
	
	Dim objFn
	Set objFn = objArgs.Item(1)
	CheckType objFn, TYPES.PROCEDURE
	' If objFn.IsSpecial Or objFn.IsMacro Then
	' 	Err.Raise vbObjectError, _
	' 		"MApply", "Need a function."
	' End If

	Dim objAST
	Set objAST = NewMalList(Array(objFn))
	Dim i
	For i = 2 To objArgs.Count - 2
		objAST.Add objArgs.Item(i)
	Next

	Dim objSeq
	Set objSeq = objArgs.Item(objArgs.Count - 1)
	CheckListOrVec objSeq

	For i = 0 To objSeq.Count - 1
		objAST.Add objSeq.Item(i)
	Next
	
	Set varRes = objFn.ApplyWithoutEval(objAST, objEnv)
	Set MApply = varRes
End Function
objNS.Add NewMalSym("apply"), NewVbsProc("MApply", False)

Function MMap(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 2
	Dim objFn, objSeq
	Set objFn = objArgs.Item(1)
	Set objSeq = objArgs.Item(2)
	CheckType objFn, TYPES.PROCEDURE
	CheckListOrVec objSeq
	If objFn.IsSpecial Or objFn.IsMacro Then
		Err.Raise vbObjectError, _
			"MApply", "Need a function."
	End If

	Set varRes = NewMalList(Array())
	Dim i
	For i = 0 To objSeq.Count - 1
		varRes.Add objFn.ApplyWithoutEval(NewMalList(Array( _
			objFn, objSeq.Item(i))), objEnv)
	Next

	Set MMap = varRes
End Function
objNS.Add NewMalSym("map"), NewVbsProc("MMap", False)

Function MIsSymbol(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Set varRes = NewMalBool(objArgs.Item(1).Type = TYPES.SYMBOL)
	Set MIsSymbol = varRes
End Function
objNS.Add NewMalSym("symbol?"), NewVbsProc("MIsSymbol", False)

Function MSymbol(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.STRING
	Set varRes = NewMalSym(objArgs.Item(1).Value)
	Set MSymbol = varRes
End Function
objNS.Add NewMalSym("symbol"), NewVbsProc("MSymbol", False)

Function MKeyword(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Select Case objArgs.Item(1).Type
		Case TYPES.STRING
			Set varRes = NewMalKwd(":" + objArgs.Item(1).Value)
		Case TYPES.KEYWORD
			Set varRes = objArgs.Item(1)
		Case Else
			Err.Raise vbObjectError, _
				"MKeyword", "Unexpect argument(s)."
	End Select
	Set MKeyword = varRes
End Function
objNS.Add NewMalSym("keyword"), NewVbsProc("MKeyword", False)

Function MIsKeyword(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Set varRes = NewMalBool(objArgs.Item(1).Type = TYPES.KEYWORD)
	Set MIsKeyword = varRes
End Function
objNS.Add NewMalSym("keyword?"), NewVbsProc("MIsKeyword", False)

Function MIsSeq(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Set varRes = NewMalBool( _
		objArgs.Item(1).Type = TYPES.LIST Or _
		objArgs.Item(1).Type = TYPES.VECTOR)
	Set MIsSeq = varRes
End Function
objNS.Add NewMalSym("sequential?"), NewVbsProc("MIsSeq", False)

Function MIsVec(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Set varRes = NewMalBool(objArgs.Item(1).Type = TYPES.VECTOR)
	Set MIsVec = varRes
End Function
objNS.Add NewMalSym("vector?"), NewVbsProc("MIsVec", False)

Function MIsMap(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Set varRes = NewMalBool(objArgs.Item(1).Type = TYPES.HASHMAP)
	Set MIsMap = varRes
End Function
objNS.Add NewMalSym("map?"), NewVbsProc("MIsMap", False)

Function MHashMap(objArgs, objEnv)
	Dim varRes
	If objArgs.Count Mod 2 <> 1 Then
		Err.Raise vbObjectError, _
			"MHashMap", "Unexpect argument(s)."
	End If
	Set varRes = NewMalMap(Array(), Array())
	Dim i
	For i = 1 To objArgs.Count - 1 Step 2
		varRes.Add objArgs.Item(i), objArgs.Item(i + 1)
	Next
	Set MHashMap = varRes
End Function
objNS.Add NewMalSym("hash-map"), NewVbsProc("MHashMap", False)

Function MAssoc(objArgs, objEnv)
	Dim varRes
	If objArgs.Count - 1 < 3 Or objArgs.Count Mod 2 <> 0 Then
		Err.Raise vbObjectError, _
			"MHashMap", "Unexpect argument(s)."
	End If
	
	Dim objMap
	Set objMap = objArgs.Item(1)
	CheckType objMap, TYPES.HASHMAP

	Dim i
	Set varRes = NewMalMap(Array(), Array())
	For Each i In objMap.Keys
		varRes.Add i, objMap.Item(i)
	Next
	For i = 2 To objArgs.Count - 1 Step 2
		varRes.Add objArgs.Item(i), objArgs.Item(i + 1)
	Next
	Set MAssoc = varRes
End Function
objNS.Add NewMalSym("assoc"), NewVbsProc("MAssoc", False)

Function MGet(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 2
	
	If objArgs.Item(1).Type = TYPES.NIL Then
		Set varRes = NewMalNil()
	Else
		CheckType objArgs.Item(1), TYPES.HASHMAP
		If objArgs.Item(1).Exists(objArgs.Item(2)) Then
			Set varRes = objArgs.Item(1).Item(objArgs.Item(2))
		Else
			Set varRes = NewMalNil()
		End If
	End If
	
	Set MGet = varRes
End Function
objNS.Add NewMalSym("get"), NewVbsProc("MGet", False)

Function MDissoc(objArgs, objEnv)
	Dim varRes
	'CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.HASHMAP
	
	If objArgs.Item(1).Exists(objArgs.Item(2)) Then
		Set varRes = NewMalMap(Array(), Array())
		
		Dim i
		Dim j, boolFlag
		For Each i In objArgs.Item(1).Keys
			boolFlag = True
			For j = 2 To objArgs.Count - 1
				If i.Type = objArgs.Item(j).Type And _
					i.Value = objArgs.Item(j).Value Then
					boolFlag = False
				End If
			Next
			If boolFlag Then
				varRes.Add i, objArgs.Item(1).Item(i)
			End If
		Next
	Else
		Set varRes = objArgs.Item(1)
	End If

	Set MDissoc = varRes
End Function
objNS.Add NewMalSym("dissoc"), NewVbsProc("MDissoc", False)

Function MKeys(objArgs, objEnv)
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.HASHMAP
	Set MKeys = NewMalList(objArgs.Item(1).Keys)
End Function
objNS.Add NewMalSym("keys"), NewVbsProc("MKeys", False)

Function MIsContains(objArgs, objEnv)
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.HASHMAP

	Set MIsContains = NewMalBool(objArgs.Item(1).Exists(objArgs.Item(2)))
End Function
objNS.Add NewMalSym("contains?"), NewVbsProc("MIsContains", False)

Function MReadLine(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	CheckType objArgs.Item(1), TYPES.STRING

	Dim strInput
	IO.Write objArgs.Item(1).Value
	On Error Resume Next
		strInput = IO.ReadLine
		If Err.Number <> 0 Then
			Set varRes = NewMalNil()
		Else
			Set varRes = NewMalStr(strInput)
		End If
	On Error Goto 0
	Set MReadLine = varRes
End Function
objNS.Add NewMalSym("readline"), NewVbsProc("MReadLine", False)

Function MTimeMs(objArgs, objEnv)
	Set MTimeMs = NewMalNum(CLng(Timer * 1000))
End Function
objNS.Add NewMalSym("time-ms"), NewVbsProc("MTimeMs", False)

Function MIsStr(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Set MIsStr = NewMalBool(objArgs.Item(1).Type = TYPES.STRING)
End Function
objNS.Add NewMalSym("string?"), NewVbsProc("MIsStr", False)

Function MIsNum(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Set MIsNum = NewMalBool(objArgs.Item(1).Type = TYPES.NUMBER)
End Function
objNS.Add NewMalSym("number?"), NewVbsProc("MIsNum", False)

Function MIsFn(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Dim varRes
	varRes = objArgs.Item(1).Type = TYPES.PROCEDURE
	If varRes Then
		varRes = (Not objArgs.Item(1).IsMacro) And _
			(Not objArgs.Item(1).IsSpecial)
	End If
	
	Set MIsFn = NewMalBool(varRes)
End Function
objNS.Add NewMalSym("fn?"), NewVbsProc("MIsFn", False)


Function MIsMacro(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Dim varRes
	varRes = objArgs.Item(1).Type = TYPES.PROCEDURE
	If varRes Then
		varRes = objArgs.Item(1).IsMacro And _
			(Not objArgs.Item(1).IsSpecial)
	End If
	
	Set MIsMacro = NewMalBool(varRes)
End Function
objNS.Add NewMalSym("macro?"), NewVbsProc("MIsMacro", False)


Function MMeta(objArgs, objEnv)
	CheckArgNum objArgs, 1
	'CheckType objArgs.Item(1), TYPES.PROCEDURE

	Dim varRes
	Set varRes = GetMeta(objArgs.Item(1))
	Set MMeta = varRes
End Function
objNS.Add NewMalSym("meta"), NewVbsProc("MMeta", False)

Function MWithMeta(objArgs, objEnv)
	CheckArgNum objArgs, 2
	'CheckType objArgs.Item(1), TYPES.PROCEDURE

	Dim varRes
	Set varRes = SetMeta(objArgs.Item(1), objArgs.Item(2))
	Set MWithMeta = varRes
End Function
objNS.Add NewMalSym("with-meta"), NewVbsProc("MWithMeta", False)

Function MConj(objArgs, objEnv)
	If objArgs.Count - 1 < 1 Then
		Err.Raise vbObjectError, _
			"MConj", "Need more arguments."
	End If
	Dim varRes
	Dim objSeq
	Set objSeq = objArgs.Item(1)
	Dim i
	Select Case objSeq.Type
		Case TYPES.LIST
			Set varRes = NewMalList(Array())
			For i = objArgs.Count - 1 To 2 Step -1
				varRes.Add objArgs.Item(i)
			Next
			For i = 0 To objSeq.Count - 1
				varRes.Add objSeq.Item(i)
			Next
		Case TYPES.VECTOR
			Set varRes = NewMalVec(Array())
			For i = 0 To objSeq.Count - 1
				varRes.Add objSeq.Item(i)
			Next
			For i = 2 To objArgs.Count - 1
				varRes.Add objArgs.Item(i)
			Next
		Case Else	
			Err.Raise vbObjectError, _
				"MConj", "Unexpect argument type."
	End Select
	Set MConj = varRes
End Function
objNS.Add NewMalSym("conj"), NewVbsProc("MConj", False)

Function MSeq(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Dim objSeq
	Set objSeq = objArgs.Item(1)
	Dim varRes
	Dim i
	Select Case objSeq.Type
		Case TYPES.STRING
			If objSeq.Value = "" Then
				Set varRes = NewMalNil()
			Else
				Set varRes = NewMalList(Array())
				For i = 1 To Len(objSeq.Value)
					varRes.Add NewMalStr(Mid(objSeq.Value, i, 1))
				Next
			End If
		Case TYPES.LIST
			If objSeq.Count = 0 Then
				Set varRes = NewMalNil()
			Else
				Set varRes = objSeq
			End If
		Case TYPES.VECTOR
			If objSeq.Count = 0 Then
				Set varRes = NewMalNil()
			Else
				Set varRes = NewMalList(Array())
				For i = 0 To objSeq.Count - 1
					varRes.Add objSeq.Item(i)
				Next
			End If
		Case TYPES.NIL
			Set varRes = NewMalNil()
		Case Else
			Err.Raise vbObjectError, _
				"MSeq", "Unexpect argument type."
	End Select
	Set MSeq = varRes
End Function
objNS.Add NewMalSym("seq"), NewVbsProc("MSeq", False)

