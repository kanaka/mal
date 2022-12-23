Option Explicit

Function PrintMalType(objMal, boolReadable)
	Dim varResult

	varResult = ""

	If TypeName(objMal) = "Nothing" Then
		PrintMalType = ""
		Exit Function
	End If
	
	Dim i
	Select Case objMal.Type
		Case TYPES.LIST
			With ValueOf(objMal)
				For i = 0 To .Count - 2
					varResult = varResult & _
						PrintMalType(.Item(i), boolReadable) & " "
				Next
				If .Count > 0 Then
					varResult = varResult & _
						PrintMalType(.Item(.Count - 1), boolReadable)
				End If
			End With
			varResult = "(" & varResult & ")"
		Case TYPES.VECTOR
			With ValueOf(objMal)
				For i = 0 To .Count - 2
					varResult = varResult & _
						PrintMalType(.Item(i), boolReadable) & " "
				Next
				If .Count > 0 Then
					varResult = varResult & _
						PrintMalType(.Item(.Count - 1), boolReadable)
				End If
			End With
			varResult = "[" & varResult & "]"
		Case TYPES.HASHMAP
			With ValueOf(objMal)
				Dim arrKeys
				arrKeys = .Keys
				For i = 0 To .Count - 2
					varResult = varResult & _
						PrintMalType(arrKeys(i), boolReadable) & " " & _
						PrintMalType(.Item(arrKeys(i)), boolReadable) & " "
				Next
				If .Count > 0 Then
					varResult = varResult & _
						PrintMalType(arrKeys(.Count - 1), boolReadable) & " " & _
						PrintMalType(.Item(arrKeys(.Count - 1)), boolReadable)
				End If
			End With
			varResult = "{" & varResult & "}"
		Case TYPES.STRING
			If boolReadable Then
				varResult = EscapeString(ValueOf(objMal))
			Else
				varResult = ValueOf(objMal)
			End If
		Case TYPES.BOOLEAN
			If ValueOf(objMal) Then
				varResult = "true"
			Else
				varResult = "false"
			End If
		Case TYPES.NIL
			varResult = "nil"
		Case TYPES.NUMBER
			varResult = CStr(ValueOf(objMal))
		Case TYPES.LAMBDA
			varResult = "#<function>"
		Case TYPES.PROCEDURE
			varResult = "#<function>"
		Case TYPES.KEYWORD
			varResult = ValueOf(objMal)
		Case TYPES.SYMBOL
			varResult = ValueOf(objMal)
		Case Else
			Err.Raise vbObjectError, _
				"PrintMalType", "unknown type"
	End Select

	PrintMalType = varResult
End Function

Function EscapeString(strRaw)
	EscapeString = strRaw
	EscapeString = Replace(EscapeString, "\", "\\")
	EscapeString = Replace(EscapeString, vbCrLf, "\n")
	EscapeString = Replace(EscapeString, """", "\""")
	EscapeString = """" & EscapeString & """"
End Function
