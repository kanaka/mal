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
            Dim args As String() = Environment.GetCommandLineArgs()

            If args.Length > 1 AndAlso args(1) = "--raw" Then
                Mal.readline.SetMode(Mal.readline.Modes.Raw)
            End If

            ' repl loop
            Dim line As String
            Do
                line = Mal.readline.Readline("user> ")
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
