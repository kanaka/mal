Function READ(str)
    READ = str
End Function

Function EVAL(str)
    EVAL = str
End Function

Function PRINT(str)
    PRINT = str
End Function

Function rep(str)
    rep = PRINT(EVAL(READ(str)))
End Function

While True
    WScript.StdOut.Write("user> ")
    code = WScript.StdIn.ReadLine()
    WScript.Echo(rep(code))
WEnd
