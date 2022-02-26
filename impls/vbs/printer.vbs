Function pr_str(o,print_readably)
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
	elseif o.type_ = "hash-map" Then
		pr_str = "{"
		bool = False
		For each item in o.value_
			bool = True
			pr_str =pr_str & pr_str(item,print_readably) & " " & pr_str(o.value_.item(item),print_readably) & " "
		Next
		if bool then
			pr_str = left(pr_str,len(pr_str)-1) & "}"
		else
			pr_str = "{}"
		End If
	Else
		if print_readably and o.type_="string" then
			pr_str = o.value_
			pr_str = replace(pr_str,"\","\\")
			pr_str = replace(pr_str,vbnewline,"\n")
			pr_str = replace(pr_str,"""","\""")
			pr_str = """" & pr_str & """"
		Elseif o.type_="string" then
			pr_str = """" & o.value_ & """"
		else
			pr_str = o.value_
		End If
	End If
	
End Function


' set list = CreateObject("System.Collections.ArrayList")
' list.add(3)
' for each i in list
' msgbox i
' next

