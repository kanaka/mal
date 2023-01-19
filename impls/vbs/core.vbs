Include "Types.vbs"

Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub


' Public objCoreNS
' Set objCoreNS = CreateObject("Scripting.Dictionary")
' objCoreNS.Add "+", GetRef("Add")
' objCoreNS.Add "-", GetRef("Subtract")
' objCoreNS.Add "*", GetRef("Multiply")
' objCoreNS.Add "/", GetRef("Divide")
' objCoreNS.Add "list", GetRef("mMakeList")
' objCoreNS.Add "list?", GetRef("mIsList") '1
' objCoreNS.Add "empty?", GetRef("mIsListEmpty") '1
' objCoreNS.Add "count", GetRef("mListCount") '1
' objCoreNS.Add "=", GetRef("mEqual") '2 'both type & value
' objCoreNS.Add "<", GetRef("mLess") '2 'number only
' objCoreNS.Add ">", GetRef("mGreater") '2 'number only
' objCoreNS.Add "<=", GetRef("mEqualLess") '2 'number only
' objCoreNS.Add ">=", GetRef("mEqualGreater") '2 'number only
' objCoreNS.Add "pr-str", GetRef("mprstr") 'all 'ret str 'readable 'concat by space
' objCoreNS.Add "str", GetRef("mstr") 'all 'ret str '!readable 'concat by ""
' objCoreNS.Add "prn", GetRef("mprn") 'all 'to screen ret nil 'concat by space 'readable
' objCoreNS.Add "println", GetRef("mprintln") 'all 'to screen ret nil 'concat by space '!readable
' objCoreNS.Add "get", GetRef("mGet")
' objCoreNS.Add "set", GetRef("mSet")
' objCoreNS.Add "first", GetRef("mFirst")
' objCoreNS.Add "last", GetRef("mLast")

' Function mLast(objArgs)
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_LIST
' 	set objRes.value = createobject("system.collections.arraylist")
' 	for i = 1 to objArgs.value.item(1).value.count - 1
' 		objRes.value.add objArgs.value.item(1).value.item(i)
' 	next
' 	Set mLast= objRes
' End Function

' Function mFirst(objArgs)
' 	'Set objRes = New MalType
' 	Set objRes = objArgs.value.item(1).value.item(0)
' 	Set mFirst= objRes
' 	'msgbox 1
' End Function

' Function mGet(objArgs)
' 	Set objRes = New MalType
' 	'objRes.Type = 
' 	Set objList = objArgs.value.item(1)
' 	numIndex = objArgs.value.item(2).value
' 	Set objRes = objList.value.Item(numIndex)
' 	'MsgBox objRes.type
' 	Set mGet = objRes
' End Function

' Function mSet(objArgs)
' 	Set objRes = New MalType
' 	'objRes.Type = 
' 	'MsgBox 1
' 	Set objList = objArgs.value.item(1)
' 	numIndex = objArgs.value.item(2).value
' 	'MsgBox numIndex
' 	Set objReplace = objArgs.value.item(3)
' 	Set objList.value.Item(numIndex) = objReplace
' 	'MsgBox objRes.type
' 	Set mSet = New MalType
' 	mSet.Type = TYPE_NIL
' End Function

' Function mprintln(objArgs)
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_NIL
' 	For i = 1 To objArgs.Value.Count - 2
' 		wsh.stdout.write PrintMalType(objArgs.Value.Item(i), False) & " "
' 	Next
' 	If objArgs.Value.Count - 1 > 0 Then
' 		wsh.stdout.write PrintMalType(objArgs.Value.Item(objArgs.Value.Count - 1), False)
' 	End If
' 	Set mprintln=objRes
' End Function

' Function mprn(objArgs)
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_NIL
' 	For i = 1 To objArgs.Value.Count - 2
' 		wsh.stdout.write PrintMalType(objArgs.Value.Item(i), True) & " "
' 	Next
' 	If objArgs.Value.Count - 1 > 0 Then
' 		wsh.stdout.write PrintMalType(objArgs.Value.Item(objArgs.Value.Count - 1), True)
' 	End If
' 	Set mprn=objRes
' End Function

' Function mstr(objArgs)
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_STRING
' 	objRes.Value = ""
' 	For i = 1 To objArgs.Value.Count - 1
' 		objRes.Value = objRes.Value & PrintMalType(objArgs.Value.Item(i), False)
' 	Next
' 	Set mstr=objRes
' End Function

' Function mprstr(objArgs)
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_STRING
' 	objRes.Value = ""
' 	For i = 1 To objArgs.Value.Count - 2
' 		objRes.Value = objRes.Value & PrintMalType(objArgs.Value.Item(i), True) & " "
' 	Next
' 	If objArgs.Value.Count - 1 > 0 Then
' 		objRes.Value = objRes.Value & PrintMalType(objArgs.Value.Item(objArgs.Value.Count - 1), True)
' 	End If
' 	Set mprstr=objRes
' End Function

' Function mEqualGreater(objArgs)
' 	CheckArgNum objArgs, 2
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Value >= objArgs.Value.Item(2).Value)
' 	Set mEqualGreater = objRes
' End Function

' Function mEqualLess(objArgs)
' 	CheckArgNum objArgs, 2
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Value <= objArgs.Value.Item(2).Value)
' 	Set mEqualLess = objRes
' End Function

' Function mGreater(objArgs)
' 	CheckArgNum objArgs, 2
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Value > objArgs.Value.Item(2).Value)
' 	Set mGreater = objRes
' End Function


' Function mLess(objArgs)
' 	CheckArgNum objArgs, 2
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Value < objArgs.Value.Item(2).Value)
' 	Set mLess = objRes
' End Function


' Function mEqual(objArgs)
' 	CheckArgNum objArgs, 2
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Type = objArgs.Value.Item(2).Type) Or _
' 		((objArgs.Value.Item(1).Type = TYPE_LIST Or objArgs.Value.Item(1).Type = TYPE_VECTOR) And _
' 		(objArgs.Value.Item(2).Type = TYPE_LIST Or objArgs.Value.Item(2).Type = TYPE_VECTOR))
' 	If objRes.Value Then
' 		'MsgBox objArgs.Value.Item(1).Type
' 		If objArgs.Value.Item(1).Type = TYPE_LIST Or objArgs.Value.Item(1).Type = TYPE_VECTOR Then
' 			objRes.Value = _
' 				(objArgs.Value.Item(1).Value.Count = objArgs.Value.Item(2).Value.Count)
' 			If objRes.Value Then
' 				Dim objTemp
' 				For i = 0 To objArgs.Value.Item(1).Value.Count - 1
' 					'an ugly recursion
				
' 					'MsgBox objArgs.Value.Item(1).Value.Item(i).type
' 					Set objTemp = New MalType
' 					objTemp.Type = TYPE_LIST
' 					Set objTemp.Value = CreateObject("System.Collections.Arraylist")
' 					objTemp.Value.Add Null
' 					objTemp.Value.Add objArgs.Value.Item(1).Value.Item(i)
' 					objTemp.Value.Add objArgs.Value.Item(2).Value.Item(i)
					
' 					objRes.Value = objRes.Value And mEqual(objTemp).Value
' 				Next
' 			End If
' 		Else
' 			'MsgBox objArgs.Value.Item(1).Value
' 			'MsgBox objArgs.Value.Item(2).Value
' 			objRes.Value = _
' 				(objArgs.Value.Item(1).Value = objArgs.Value.Item(2).Value)
' 		End If
' 	End If
' 	Set mEqual = objRes
' End Function

' Sub Er(sInfo)
' 	boolError = True
' 	strError = sInfo
' End Sub

' Function mListCount(objArgs)
' 	CheckArgNum objArgs, 1
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_NUMBER
' 	If objArgs.Value.Item(1).Type = TYPE_LIST Then
' 		objRes.Value = objArgs.Value.Item(1).Value.Count
' 	ElseIf objArgs.Value.Item(1).Type = TYPE_NIL Then
' 		objRes.Value = 0
' 	Else
' 		Er "can't count"
' 	End If
' 	Set mListCount = objRes
' End Function

' Function mIsListEmpty(objArgs)
' 	CheckArgNum objArgs, 1
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Value.Count = 0)
' 	Set mIsListEmpty = objRes
' End Function

' Function mIsList(objArgs)
' 	CheckArgNum objArgs, 1
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_BOOLEAN
' 	objRes.Value = (objArgs.Value.Item(1).Type = TYPE_LIST)
' 	Set mIsList = objRes
' End Function

' Function mMakeList(objArgs)
' 	Dim objRes,i
' 	Set objRes = New MalType
' 	objRes.Type = TYPE_LIST
' 	Set objRes.Value = CreateObject("System.Collections.ArrayList")
' 	For i = 1 To objArgs.Value.Count - 1
' 		objRes.Value.Add objArgs.Value.Item(i)
' 	Next
' 	Set mMakeList = objRes
' End Function

' Function Add(objArgs)
' 	CheckArgNum objArgs, 2
' 	Set Add = New MalType
' 	Add.Type = TYPE_NUMBER
' 	Add.Value = objArgs.Value.Item(1).Value + objArgs.Value.Item(2).Value
' End Function

' Function Subtract(objArgs)
' 	CheckArgNum objArgs, 2
' 	Set Subtract = New MalType
' 	Subtract.Type = TYPE_NUMBER
' 	Subtract.Value = objArgs.Value.Item(1).Value - objArgs.Value.Item(2).Value
' End Function

' Function Multiply(objArgs)
' 	CheckArgNum objArgs, 2
' 	Set Multiply = New MalType
' 	Multiply.Type = TYPE_NUMBER
' 	Multiply.Value = objArgs.Value.Item(1).Value * objArgs.Value.Item(2).Value
' End Function

' Function Divide(objArgs)
' 	CheckArgNum objArgs, 2
' 	Set Divide = New MalType
' 	Divide.Type = TYPE_NUMBER
' 	Divide.Value = objArgs.Value.Item(1).Value \ objArgs.Value.Item(2).Value
' End Function
