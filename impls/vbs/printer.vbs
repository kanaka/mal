Function pr_str(o,print_readably)
	if not print_readably then
		If left(o.type_,4) = "list" Then
			pr_str =mid(o.type_,5,1) 
			bool = False
			For Each item In o.value_
				bool = True
				pr_str =pr_str & pr_str(item,print_readably) & " "
			Next
			if bool then
				pr_str = left(pr_str,len(pr_str)-1) & mid(o.type_,6,1)
			else
				pr_str = mid(o.type_,5,2)
			End If
		Else
			pr_str = o.value_
		End If
	Else
	end if
End Function


' set list = CreateObject("System.Collections.ArrayList")
' list.add(3)
' for each i in list
' msgbox i
' next

