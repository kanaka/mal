Imports System
Imports Mono.Terminal ' LineEditor (getline.cs)

Namespace Mal
    Public Class readline
        Enum Modes
            Terminal
            Raw
        End Enum

        Public Shared mode As Modes = Modes.Terminal

        Shared lineedit As LineEditor = Nothing

        Public Shared Sub SetMode(new_mode As Modes)
            mode = new_mode
        End Sub

        Public Shared Function Readline(prompt As String) As String
            If mode = Modes.Terminal Then
                If lineedit Is Nothing Then
                    lineedit = New LineEditor("Mal")
                End If
                return lineedit.Edit(prompt, "")
            Else
                Console.Write(prompt)
                Console.Out.Flush()
                return Console.ReadLine()
            End If
        End Function
    End Class
End Namespace
