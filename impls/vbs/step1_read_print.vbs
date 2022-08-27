Option Explicit

Include "Const.vbs"
Include "Reader.vbs"
Include "Printer.vbs"

Dim strCode
While True 'REPL
	WScript.StdOut.Write("user> ")
	On Error Resume Next
	strCode = WScript.StdIn.ReadLine()
	If Err.Number <> 0 Then WScript.Quit 0
	On Error Goto 0
	WScript.Echo REP(strCode)
Wend

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