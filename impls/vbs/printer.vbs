Option Explicit

Function PrintMalType(objMal, boolReadable)
	'MsgBox 1
	PrintMalType = ""
	If TypeName(objMal) = "Nothing" Then
		Exit Function
	End If
	
	Dim i
	Select Case objMal.Type
		Case TYPE_LIST
			With objMal.Value
				For i = 0 To .Count - 2
					PrintMalType = PrintMalType & _
						PrintMalType(.Item(i), boolReadable) & " "
				Next
				If .Count > 0 Then
					PrintMalType = PrintMalType & _
						PrintMalType(.Item(.Count - 1), boolReadable)
				End If
			End With
			PrintMalType = "(" & PrintMalType & ")"
		Case TYPE_VECTOR
			With objMal.Value
				For i = 0 To .Count - 2
					PrintMalType = PrintMalType & _
						PrintMalType(.Item(i), boolReadable) & " "
				Next
				If .Count > 0 Then
					PrintMalType = PrintMalType & _
						PrintMalType(.Item(.Count - 1), boolReadable)
				End If
			End With
			PrintMalType = "[" & PrintMalType & "]"
		Case TYPE_HASHMAP
			With objMal.Value
				Dim arrKeys
				arrKeys = .Keys
				For i = 0 To .Count - 2
					PrintMalType = PrintMalType & _
						PrintMalType(arrKeys(i), boolReadable) & " " & _
						PrintMalType(.Item(arrKeys(i)), boolReadable) & " "
				Next
				If .Count > 0 Then
					PrintMalType = PrintMalType & _
						PrintMalType(arrKeys(.Count - 1), boolReadable) & " " & _
						PrintMalType(.Item(arrKeys(.Count - 1)), boolReadable)
				End If
			End With
			PrintMalType = "{" & PrintMalType & "}"
		Case TYPE_STRING
			If boolReadable Then
				PrintMalType = EscapeString(objMal.Value)
			Else
				'PrintMalType = """" & objMal.Value & """"
				PrintMalType = objMal.Value
			End If
		Case TYPE_BOOLEAN
			If objMal.Value Then
				PrintMalType = "true"
			Else
				PrintMalType = "false"
			End If
		Case TYPE_NIL
			PrintMalType = "nil"
		Case TYPE_NUMBER
			PrintMalType = CStr(objMal.Value)
		Case TYPE_FUNCTION
			PrintMalType = "#<function>"
		Case Else
			PrintMalType = objMal.Value
	End Select
End Function

Function EscapeString(strRaw)
	EscapeString = strRaw
	EscapeString = Replace(EscapeString, "\", "\\")
	EscapeString = Replace(EscapeString, vbCrLf, "\n")
	EscapeString = Replace(EscapeString, """", "\""")
	EscapeString = """" & EscapeString & """"
End Function