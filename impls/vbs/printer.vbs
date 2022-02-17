Function pr_str(o)
	If typename(o) = "ArrayList" Then
		pr_str ="(" 
		bool = False
		For Each item In o
			bool = True
			pr_str =pr_str & pr_str(item) & " "
		Next
		if bool then
			pr_str = left(pr_str,len(pr_str)-1) & ")"
		else
			pr_str = "()"
		End If
	Else
		pr_str = o.value_
	End If
End Function


' set list = CreateObject("System.Collections.ArrayList")
' list.add(3)
' for each i in list
' msgbox i
' next

