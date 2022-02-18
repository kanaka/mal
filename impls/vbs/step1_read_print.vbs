Include "reader.vbs"
Include "printer.vbs"

Function READ(str)
    set READ = read_str(str)
End Function

Function EVAL(oMal)
    set EVAL = oMal
End Function

Function PRINT(oMal)
    PRINT = pr_str(oMal,false)
End Function

Function rep(str)
	on error resume next
    rep = PRINT(EVAL(READ(str)))
	if err.number <> 0 then rep = err.description
	on error goto 0
End Function

While True
    WScript.StdOut.Write("user> ")
    code = WScript.StdIn.ReadLine()
    WScript.Echo(rep(code))
WEnd

Sub Include(sInstFile) 
	Dim oFSO, f, s 
	Set oFSO = CreateObject("Scripting.FileSystemObject")
    sInstFile = oFSO.GetParentFolderName(oFSO.GetFile(Wscript.ScriptFullName)) & "\" & sInstFile
	Set f = oFSO.OpenTextFile(sInstFile) 
	s = f.ReadAll 
	f.Close 
	Set f = Nothing
	Set oFSO = Nothing
	ExecuteGlobal s 
End Sub