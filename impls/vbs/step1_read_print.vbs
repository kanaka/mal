Option Explicit

Include "IO.vbs"
Include "Types.vbs"
Include "Reader.vbs"
Include "Printer.vbs"

Call REPL()
Sub REPL()
	Dim strCode
	While True
		IO.Write "user> "

		On Error Resume Next
			strCode = IO.ReadLine
			If Err.Number <> 0 Then WScript.Quit 0
		On Error Goto 0

		Dim strRes
		On Error Resume Next
			strRes = REP(strCode)
			If Err.Number <> 0 Then
				IO.WriteErrLine "Exception: " + Err.Description
			Else
				If strRes <> "" Then
					IO.WriteLine strRes
				End If
			End If
		On Error Goto 0
	Wend
End Sub

Function Read(strCode)
	Set Read = ReadString(strCode)
End Function

Function Evaluate(objCode)
	Set Evaluate = objCode
End Function

Function Print(objCode)
	Print = PrintMalType(objCode, True)
End Function

Function REP(strCode)
	REP = Print(Evaluate(Read(strCode)))
End Function

Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub