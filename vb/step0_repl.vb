Imports System
Imports Mal

Namespace Mal
    Class step0_repl
        ' read
        Shared Function READ(str As String) As String
            Return str
        End Function

        ' eval
        Shared Function EVAL(ast As String, env As String) As String
            Return ast
        End Function
        
        ' print
        Shared Function PRINT(exp As String) As String
            Return exp
        End Function

        ' repl
        Shared Function REP(str As String, env As String) As String
            Return PRINT(EVAL(READ(str), env))
        End Function

        Shared Function Main As Integer
            Dim prompt As String = "user> "
            Dim line As String

            Do
                line = Mal.readline.Readline(prompt)
                If line is Nothing Then
                    Exit Do
                End If
                If line = "" Then
                    Continue Do
                End If
                Console.WriteLine(REP(line, ""))
            Loop While True
            Return 0
        End function
    End Class
End Namespace
