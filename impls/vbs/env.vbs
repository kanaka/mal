Option Explicit

Function NewEnv(objOuter)
	Set NewEnv = New Environment
	Set NewEnv.Outer = objOuter
End Function

Class Environment
	Public objOuter
	Public objBinds
	Private Sub Class_Initialize()
		Set objBinds = CreateObject("Scripting.Dictionary")
		Set objOuter = Nothing
	End Sub
	
	Public Property Set Outer(objEnv)
		Set objOuter = objEnv
	End Property

	Public Sub Add(varKey, varValue)
		Set objBinds.Item(varKey) = varValue
	End Sub

	Public Function [Get](varKey)
		Dim objEnv, varRet
		Set objEnv = Me
		Do
			If objEnv.objBinds.Exists(varKey) Then
				Set varRet = objEnv.objBinds(varKey)
				Exit Do
			End If
			Set objEnv = objEnv.objOuter
			If TypeName(objEnv) = "Nothing" Then
				Set varRet = Nothing
				Exit Do
			End If
		Loop

		Set [Get] = varRet
	End Function
End Class