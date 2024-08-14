Class IOWrap
	Public NoStdErr
	Public EchoStdIn

	Private Sub Class_Initialize
		With WScript.CreateObject("WScript.Shell")
			NoStdErr = CBool(.ExpandEnvironmentStrings("%MAL_VBS_IMPL_NO_STDERR%"))
			EchoStdIn = CBool(.ExpandEnvironmentStrings("%MAL_VBS_IMPL_ECHO_STDIN%"))
		End With
	End Sub

	Public Sub Write(sText)
		WScript.StdOut.Write sText
	End Sub

	Public Sub WriteLine(sText)
		WScript.StdOut.WriteLine sText
	End Sub

	Public Function ReadLine()
		ReadLine = WScript.StdIn.ReadLine
		If EchoStdIn Then
			WScript.StdOut.WriteLine ReadLine
		End If
	End Function

	Public Sub WriteErr(sText)
		If Not NoStdErr Then
			WScript.StdErr.Write sText
		Else ' Redirect to StdOut
			WScript.StdOut.Write sText
		End If
	End Sub

	Public Sub WriteErrLine(sText)
		If Not NoStdErr Then
			WScript.StdErr.WriteLine sText
		Else ' Redirect to StdOut
			WScript.StdOut.WriteLine sText
		End If
	End Sub
End Class

Set IO = New IOWrap