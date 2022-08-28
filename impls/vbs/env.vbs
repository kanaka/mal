
class Environment
	Private objOuterEnv
	Public objBindings
	Private objSelf
	Private Sub Class_Initialize()
		Set objBindings = CreateObject("Scripting.Dictionary")
		Set objOuterEnv = Nothing
		Set objSelf = Nothing
	End Sub
	
	Public Sub Init(objBinds, objExpressions)
		'MsgBox objExpressions.type
		Dim i
		For i = 0 To objBinds.Value.Count - 1
			Add objBinds.Value.Item(i).Value, _
				Evaluate(objExpressions.Value.Item(i+1), objSelf)
			'wsh.echo objBinds.Value.Item(i).Value
			'wsh.echo objExpressions.Value.Item(i).type
			'wsh.echo TypeName(Evaluate(objExpressions.Value.Item(i), objSelf))
			'wsh.echo Evaluate(objExpressions.Value.Item(i), objSelf).type
		Next
		'MsgBox objBindings("a")
	End Sub
	
	Public Function SetOuter(objEnv)
		Set objOuterEnv = objEnv
	End Function
	
	Public Function SetSelf(objEnv)
		Set objSelf = objEnv
	End Function

	Public Sub Add(varKey, varValue)
		'objBindings.Add varKey, varValue
		Set objBindings(varKey) = varValue
	End Sub

	Public Function Find(varKey)
		If objBindings.Exists(varKey) Then
			Set Find = objSelf
		Else
			If TypeName(objOuterEnv) <> "Nothing" Then
				Set Find = objOuterEnv.Find(varKey)
			Else
				boolError = True
				strError = "symbol " & varKey & " not found"
				Call REPL()
			End If
		End If
	End Function
	
	Public Function [Get](varKey)
		Set [Get] = Find(varKey).objBindings(varKey)
	End Function
end class