Option Explicit

Function NewEnv(objOuter)
	Dim varRet
	Set varRet = New Environment
	Set varRet.Self = varRet
	Set varRet.Outer = objOuter
	Set NewEnv = varRet
End Function

Class Environment
	Private objOuter, objSelf
	Private objBinds
	Private Sub Class_Initialize()
		Set objBinds = CreateObject("Scripting.Dictionary")
		Set objOuter = Nothing
		Set objSelf = Nothing
	End Sub
	
	Public Property Set Outer(objEnv)
		Set objOuter = objEnv
	End Property

	Public Property Set Self(objEnv)
		Set objSelf = objEnv
	End Property

	' Public objBindings
	' Public Sub Init(objBinds, objExpressions)
	' 	Dim boolVarLen
	' 	boolVarLen = False

	' 	Dim i
	' 	For i = 0 To objBinds.Value.Count - 1
	' 		If objBinds.Value.Item(i).Value = "&" Then flag=True
	' 		If flag Then
	' 			'assume i+1 = objBinds.Value.Count - 1
	' 			Dim oTmp
	' 			Set oTmp = New MalType
	' 			oTmp.Type = TYPE_LIST
	' 			Set oTmp.Value = CreateObject("System.Collections.ArrayList")
	' 			Dim j
	' 			For j = i+1 To objExpressions.Value.Count - 1
	' 				oTmp.Value.Add Evaluate(objExpressions.Value.Item(j), objSelf)
	' 			Next
	' 			'MsgBox objBinds.Value.Item(i+1)
	' 			Add objBinds.Value.Item(i+1).Value, oTmp
	' 			Exit For
	' 		Else
	' 			Add objBinds.Value.Item(i).Value, _
	' 				Evaluate(objExpressions.Value.Item(i+1), objSelf)	
	' 		End If
	' 		'wsh.echo objBinds.Value.Item(i).Value
	' 		'wsh.echo objExpressions.Value.Item(i).type
	' 		'wsh.echo TypeName(Evaluate(objExpressions.Value.Item(i), objSelf))
	' 		'wsh.echo Evaluate(objExpressions.Value.Item(i), objSelf).type
	' 	Next
	' 	'MsgBox objBindings("a")
	' End Sub
	

	Public Sub Add(varKey, varValue)
		Set objBinds.Item(varKey.Value) = varValue
	End Sub

	Public Function Find(varKey)
		Dim varRet
		If objBinds.Exists(varKey.Value) Then
			Set varRet = objSelf
		Else
			If TypeName(objOuter) <> "Nothing" Then
				Set varRet = objOuter.Find(varKey)
			Else
				Err.Raise vbObjectError, _
					"Environment", "Symbol '" + varKey.Value + "' not found."
			End If
		End If

		Set Find = varRet
	End Function
	
	Public Function [Get](varKey)
		Dim objEnv, varRet
		Set objEnv = Find(varKey)
		If objEnv Is objSelf Then
			Set varRet = objBinds(varKey.Value)
		Else
			Set varRet = objEnv.Get(varKey)
		End If
		
		Set [Get] = varRet
	End Function
End Class