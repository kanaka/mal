Option Explicit

Include "IO.vbs"

Function Read(strCode)
	Read = strCode
End Function

Function Evaluate(strCode)
	Evaluate = strCode
End Function

Function Print(strCode)
	Print = strCode
End Function

Function REP(strCode)
	REP = Print(Evaluate(Read(strCode)))
End Function

Dim strCode
While True 'REPL
	WScript.StdOut.Write "user> "
	On Error Resume Next
		strCode = IO.ReadLine
		If Err.Number <> 0 Then WScript.Quit 0
	On Error Goto 0
	
	IO.WriteLine REP(strCode)
Wend


Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub