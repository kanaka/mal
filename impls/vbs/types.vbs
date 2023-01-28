Option Explicit

Dim TYPES
Set TYPES = New MalTypes

Class MalTypes
	Public LIST, VECTOR, HASHMAP, [BOOLEAN], NIL
	Public KEYWORD, [STRING], NUMBER, SYMBOL
	Public PROCEDURE, ATOM

	Public [TypeName]
	Private Sub Class_Initialize
		[TypeName] = Array( _
				"LIST", "VECTOR", "HASHMAP", "BOOLEAN", _
				"NIL", "KEYWORD", "STRING", "NUMBER", _
				"SYMBOL", "PROCEDURE", "ATOM")

		Dim i
		For i = 0 To UBound([TypeName])
			Execute "[" + [TypeName](i) + "] = " + CStr(i)
		Next
	End Sub
End Class

Class MalType
	Public [Type]
	Public Value

	Private varMeta
	Public Property Get MetaData()
		If IsEmpty(varMeta) Then
			Set MetaData = NewMalNil()
		Else
			Set MetaData = varMeta
		End If
	End Property
	
	Public Property Set MetaData(objMeta)
		Set varMeta = objMeta
	End Property
	
	Public Function Copy()
		Set Copy = NewMalType([Type], Value)
	End Function

	Public Function Init(lngType, varValue)
		[Type] = lngType
		Value = varValue
	End Function
End Class

Function NewMalType(lngType, varValue)
	Dim varResult
	Set varResult = New MalType
	varResult.Init lngType, varValue
	Set NewMalType = varResult
End Function

Function NewMalBool(varValue)
	Set NewMalBool = NewMalType(TYPES.BOOLEAN, varValue)
End Function

Function NewMalNil()
	Set NewMalNil = NewMalType(TYPES.NIL, Empty)
End Function

Function NewMalKwd(varValue)
	Set NewMalKwd = NewMalType(TYPES.KEYWORD, varValue)
End Function

Function NewMalStr(varValue)
	Set NewMalStr = NewMalType(TYPES.STRING, varValue)
End Function

Function NewMalNum(varValue)
	Set NewMalNum = NewMalType(TYPES.NUMBER, varValue)
End Function

Function NewMalSym(varValue)
	Set NewMalSym = NewMalType(TYPES.SYMBOL, varValue)
End Function

Class MalAtom
	Public [Type]
	Public Value
	
	Private varMeta
	Public Property Get MetaData()
		If IsEmpty(varMeta) Then
			Set MetaData = NewMalNil()
		Else
			Set MetaData = varMeta
		End If
	End Property
	
	Public Property Set MetaData(objMeta)
		Set varMeta = objMeta
	End Property

	Public Function Copy()
		Set Copy = NewMalAtom(Value)
	End Function

	Public Sub Reset(objMal)
		Set Value = objMal
	End Sub

	Private Sub Class_Initialize
		[Type] = TYPES.ATOM
	End Sub
End Class

Function NewMalAtom(varValue)
	Dim varRes
	Set varRes = New MalAtom
	varRes.Reset varValue
	Set NewMalAtom = varRes
End Function

Class MalList ' Extends MalType
	Public [Type]
	Public Value
	
	Private varMeta
	Public Property Get MetaData()
		If IsEmpty(varMeta) Then
			Set MetaData = NewMalNil()
		Else
			Set MetaData = varMeta
		End If
	End Property
	
	Public Property Set MetaData(objMeta)
		Set varMeta = objMeta
	End Property

	Public Function Copy()
		Set Copy = New MalList
		Set Copy.Value = Value
	End Function

	Private Sub Class_Initialize
		[Type] = TYPES.LIST
		Set Value = CreateObject("System.Collections.ArrayList")
	End Sub

	Public Function Init(arrValues)
		Dim i
		For i = 0 To UBound(arrValues)
			Add arrValues(i)
		Next
	End Function

	Public Function Add(objMalType)
		Value.Add objMalType
	End Function
	
	Public Property Get Item(i)
		Set Item = Value.Item(i)
	End Property

	Public Property Let Item(i, varValue)
		Value.Item(i) = varValue
	End Property

	Public Property Set Item(i, varValue)
		Set Value.Item(i) = varValue
	End Property

	Public Function Count()
		Count = Value.Count
	End Function
End Class

Function NewMalList(arrValues)
	Dim varResult
	Set varResult = New MalList
	varResult.Init arrValues
	Set NewMalList = varResult
End Function

Class MalVector ' Extends MalType
	Public [Type]
	Public Value
	
	Private varMeta
	Public Property Get MetaData()
		If IsEmpty(varMeta) Then
			Set MetaData = NewMalNil()
		Else
			Set MetaData = varMeta
		End If
	End Property
	
	Public Property Set MetaData(objMeta)
		Set varMeta = objMeta
	End Property

	Public Function Copy()
		Set Copy = New MalVector
		Set Copy.Value = Value
	End Function

	Private Sub Class_Initialize
		[Type] = TYPES.VECTOR
		Set Value = CreateObject("System.Collections.ArrayList")
	End Sub

	Public Function Init(arrValues)
		Dim i
		For i = 0 To UBound(arrValues)
			Add arrValues(i)
		Next
	End Function

	Public Function Add(objMalType)
		Value.Add objMalType
	End Function
	
	Public Property Get Item(i)
		Set Item = Value.Item(i)
	End Property

	Public Property Let Item(i, varValue)
		Value.Item(i) = varValue
	End Property

	Public Property Set Item(i, varValue)
		Set Value.Item(i) = varValue
	End Property

	Public Function Count()
		Count = Value.Count
	End Function
End Class

Function NewMalVec(arrValues)
	Dim varResult
	Set varResult = New MalVector
	varResult.Init arrValues
	Set NewMalVec = varResult
End Function

Class MalHashmap 'Extends MalType
	Public [Type]
	Public Value

	Private varMeta
	Public Property Get MetaData()
		If IsEmpty(varMeta) Then
			Set MetaData = NewMalNil()
		Else
			Set MetaData = varMeta
		End If
	End Property
	
	Public Property Set MetaData(objMeta)
		Set varMeta = objMeta
	End Property

	Public Function Copy()
		Set Copy = New MalHashmap
		Set Copy.Value = Value
	End Function


	Private Sub Class_Initialize
		[Type] = TYPES.HASHMAP
		Set Value = CreateObject("Scripting.Dictionary")
	End Sub

	Public Function Init(arrKeys, arrValues)
		Dim i
		For i = 0 To UBound(arrKeys)
			Add arrKeys(i), arrValues(i)
		Next
	End Function

	Private Function M2S(objKey)
		Dim varRes
		Select Case objKey.Type
			Case TYPES.STRING
				varRes = "S" + objKey.Value
			Case TYPES.KEYWORD
				varRes = "K" + objKey.Value
			Case Else
				Err.Raise vbObjectError, _
					"MalHashmap", "Unexpect key type."
		End Select
		M2S = varRes
	End Function

	Private Function S2M(strKey)
		Dim varRes
		Select Case Left(strKey, 1)
			Case "S"
				Set varRes = NewMalStr(Right(strKey, Len(strKey) - 1))
			Case "K"
				Set varRes = NewMalKwd(Right(strKey, Len(strKey) - 1))
			Case Else
				Err.Raise vbObjectError, _
					"MalHashmap", "Unexpect key type."
		End Select
		Set S2M = varRes
	End Function

	Public Function Add(varKey, varValue)
		If varKey.Type <> TYPES.STRING And _
			varKey.Type <> TYPES.KEYWORD Then
			Err.Raise vbObjectError, _
				"MalHashmap", "Unexpect key type."
		End If
		
		Set Value.Item(M2S(varKey)) = varValue
		'Value.Add M2S(varKey), varValue
	End Function
	
	Public Property Get Keys()
		Dim aKeys
		aKeys = Value.Keys
		Dim aRes()
		ReDim aRes(UBound(aKeys))
		Dim i
		For i = 0 To UBound(aRes)
			Set aRes(i) = S2M(aKeys(i))
		Next

		Keys = aRes
	End Property

	Public Function Count()
		Count = Value.Count
	End Function

	Public Property Get Item(i)
		Set Item = Value.Item(M2S(i))
	End Property

	Public Function Exists(varKey)
		If varKey.Type <> TYPES.STRING And _
			varKey.Type <> TYPES.KEYWORD Then
			Err.Raise vbObjectError, _
				"MalHashmap", "Unexpect key type."
		End If
		Exists = Value.Exists(M2S(varKey))
	End Function

	Public Property Let Item(i, varValue)
		Value.Item(M2S(i)) = varValue
	End Property

	Public Property Set Item(i, varValue)
		Set Value.Item(M2S(i)) = varValue
	End Property
End Class

Function NewMalMap(arrKeys, arrValues)
	Dim varResult
	Set varResult = New MalHashmap
	varResult.Init arrKeys, arrValues
	Set NewMalMap = varResult
End Function

Class VbsProcedure 'Extends MalType
	Public [Type]
	Public Value
	
	Public IsMacro
	Public boolSpec
	Public MetaData
	Private Sub Class_Initialize
		[Type] = TYPES.PROCEDURE
		IsMacro = False
		Set MetaData = NewMalNil()
	End Sub

	Public Property Get IsSpecial()
		IsSpecial = boolSpec
	End Property

	Public Function Init(objFunction, boolIsSpec)
		Set Value = objFunction
		boolSpec = boolIsSpec
	End Function

	Public Function Apply(objArgs, objEnv)
		Dim varResult
		If boolSpec Then
			Set varResult = Value(objArgs, objEnv)
		Else
			Set varResult = Value(EvaluateRest(objArgs, objEnv), objEnv)
		End If
		Set Apply = varResult
	End Function

	Public Function ApplyWithoutEval(objArgs, objEnv)
		Dim varResult
		Set varResult = Value(objArgs, objEnv)
		
		Set ApplyWithoutEval = varResult
	End Function

	Public Function Copy()
		Dim varRes
		Set varRes = New VbsProcedure
		varRes.Type = [Type]
		Set varRes.Value = Value
		varRes.IsMacro = IsMacro
		varRes.boolSpec = boolSpec
		Set Copy = varRes
	End Function
End Class

Function NewVbsProc(strFnName, boolSpec)
	Dim varResult
	Set varResult = New VbsProcedure
	varResult.Init GetRef(strFnName), boolSpec
	Set NewVbsProc = varResult
End Function

Class MalProcedure 'Extends MalType
	Public [Type]
	Public Value
	
	Public IsMacro

	Public Property Get IsSpecial()
		IsSpecial = False
	End Property

	Public MetaData
	Private Sub Class_Initialize
		[Type] = TYPES.PROCEDURE
		IsMacro = False
		Set MetaData = NewMalNil()
	End Sub

	Public objParams, objCode, objSavedEnv
	Public Function Init(objP, objC, objE)
		Set objParams = objP
		Set objCode = objC
		Set objSavedEnv = objE
	End Function

	Public Function Apply(objArgs, objEnv)
		If IsMacro Then
			Err.Raise vbObjectError, _
				"MalProcedureApply", "Not a procedure."
		End If

		Dim varRet
		Dim objNewEnv
		Set objNewEnv = NewEnv(objSavedEnv)
		Dim i
		i = 0
		Dim objList
		While i < objParams.Count
			If objParams.Item(i).Value = "&" Then
				If objParams.Count - 1 = i + 1 Then
					Set objList = NewMalList(Array())
					objNewEnv.Add objParams.Item(i + 1), objList
					While i + 1 < objArgs.Count
						objList.Add Evaluate(objArgs.Item(i + 1), objEnv)
						i = i + 1
					Wend
					i = objParams.Count ' Break While
				Else
					Err.Raise vbObjectError, _
						"MalProcedureApply", "Invalid parameter(s)."
				End If
			Else
				If i + 1 >= objArgs.Count Then
					Err.Raise vbObjectError, _
						"MalProcedureApply", "Need more arguments."
				End If
				objNewEnv.Add objParams.Item(i), _
					Evaluate(objArgs.Item(i + 1), objEnv)
				i = i + 1
			End If
		Wend
		
		Set varRet = EvalLater(objCode, objNewEnv)
		Set Apply = varRet
	End Function

	Public Function MacroApply(objArgs, objEnv)
		If Not IsMacro Then
			Err.Raise vbObjectError, _
				"MalMacroApply", "Not a macro."
		End If

		Dim varRet
		Dim objNewEnv
		Set objNewEnv = NewEnv(objSavedEnv)
		Dim i
		i = 0
		Dim objList
		While i < objParams.Count
			If objParams.Item(i).Value = "&" Then
				If objParams.Count - 1 = i + 1 Then
					Set objList = NewMalList(Array())
					
					' No evaluation
					objNewEnv.Add objParams.Item(i + 1), objList
					While i + 1 < objArgs.Count
						objList.Add objArgs.Item(i + 1)
						i = i + 1
					Wend
					i = objParams.Count ' Break While
				Else
					Err.Raise vbObjectError, _
						"MalMacroApply", "Invalid parameter(s)."
				End If
			Else
				If i + 1 >= objArgs.Count Then
					Err.Raise vbObjectError, _
						"MalMacroApply", "Need more arguments."
				End If
				
				' No evaluation
				objNewEnv.Add objParams.Item(i), _
					objArgs.Item(i + 1)
				i = i + 1
			End If
		Wend
		
		' EvalLater -> Evaluate
		Set varRet = Evaluate(objCode, objNewEnv)
		Set MacroApply = varRet
	End Function


	Public Function ApplyWithoutEval(objArgs, objEnv)
		Dim varRet
		Dim objNewEnv
		Set objNewEnv = NewEnv(objSavedEnv)
		Dim i
		i = 0
		Dim objList
		While i < objParams.Count
			If objParams.Item(i).Value = "&" Then
				If objParams.Count - 1 = i + 1 Then
					Set objList = NewMalList(Array())
					
					' No evaluation
					objNewEnv.Add objParams.Item(i + 1), objList
					While i + 1 < objArgs.Count
						objList.Add objArgs.Item(i + 1)
						i = i + 1
					Wend
					i = objParams.Count ' Break While
				Else
					Err.Raise vbObjectError, _
						"MalMacroApply", "Invalid parameter(s)."
				End If
			Else
				If i + 1 >= objArgs.Count Then
					Err.Raise vbObjectError, _
						"MalMacroApply", "Need more arguments."
				End If
				
				' No evaluation
				objNewEnv.Add objParams.Item(i), _
					objArgs.Item(i + 1)
				i = i + 1
			End If
		Wend
		
		' EvalLater -> Evaluate
		Set varRet = Evaluate(objCode, objNewEnv)
		Set ApplyWithoutEval = varRet
	End Function

	
	Public Function Copy()
		Dim varRes
		Set varRes = New MalProcedure
		varRes.Type = [Type]
		varRes.Value = Value
		varRes.IsMacro = IsMacro
		Set varRes.objParams = objParams
		Set varRes.objCode = objCode
		Set varRes.objSavedEnv = objSavedEnv
		Set Copy = varRes
	End Function
End Class

Function NewMalProc(objParams, objCode, objEnv)
	Dim varRet
	Set varRet = New MalProcedure
	varRet.Init objParams, objCode, objEnv
	Set NewMalProc = varRet
End Function

Function NewMalMacro(objParams, objCode, objEnv)
	Dim varRet
	Set varRet = New MalProcedure
	varRet.Init objParams, objCode, objEnv
	varRet.IsMacro = True
	Set NewMalProc = varRet
End Function

Function SetMeta(objMal, objMeta)
	Dim varRes
	Set varRes = objMal.Copy
	Set varRes.MetaData = objMeta
	Set SetMeta = varRes
End Function

Function GetMeta(objMal)
	Set GetMeta = objMal.MetaData
End Function