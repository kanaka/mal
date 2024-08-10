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
			With objMal
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
			With objMal
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
			With objMal
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
				varResult = EscapeString(objMal.Value)
			Else
				varResult = objMal.Value
			End If
		Case TYPES.BOOLEAN
			If objMal.Value Then
				varResult = "true"
			Else
				varResult = "false"
			End If
		Case TYPES.NIL
			varResult = "nil"
		Case TYPES.NUMBER
			varResult = CStr(objMal.Value)
		Case TYPES.PROCEDURE
			varResult = "#<function>"
		Case TYPES.KEYWORD
			varResult = objMal.Value
		Case TYPES.SYMBOL
			varResult = objMal.Value
		Case TYPES.ATOM
			varResult = "(atom " + PrintMalType(objMal.Value, boolReadable) + ")"
		Case Else
			Err.Raise vbObjectError, _
				"PrintMalType", "Unknown type."
	End Select

	PrintMalType = varResult
End Function

Function EscapeString(strRaw)
	EscapeString = strRaw
	EscapeString = Replace(EscapeString, "\", "\\")
	EscapeString = Replace(EscapeString, vbCrLf, vbLf)
	EscapeString = Replace(EscapeString, vbCr, vbLf)
	EscapeString = Replace(EscapeString, vbLf, "\n")
	EscapeString = Replace(EscapeString, """", "\""")
	EscapeString = """" & EscapeString & """"
End Function
