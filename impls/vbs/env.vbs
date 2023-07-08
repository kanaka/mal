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

	Public Property Get Outer()
		Set Outer = objOuter
	End Property

	Public Property Set Self(objEnv)
		Set objSelf = objEnv
	End Property
	
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
					"Environment", "'" + varKey.Value + "' not found"
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