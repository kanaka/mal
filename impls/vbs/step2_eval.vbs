Include "reader.vbs"
Include "printer.vbs"

function add(args)
	set add = new MalType
	add.type_ = "number"
	'msgbox typename(args)
	add.value_ = args.value_.item(1).value_ + args.value_.item(2).value_
end function

function subtract(args)
	set subtract = new MalType
	subtract.type_ = "number"
	subtract.value_ = args.value_.item(1).value_ - args.value_.item(2).value_
end function

function multiply(args)
	set multiply = new MalType
	multiply.type_ = "number"
	multiply.value_ = args.value_.item(1).value_ * args.value_.item(2).value_
end function

function divide(args)
	set divide = new MalType
	divide.type_ = "number"
	divide.value_ = args.value_.item(1).value_ / args.value_.item(2).value_
end function

function donothing(args)
	set donothing = new MalType
	donothing.type_ = "nil"
	donothing.value_ = "error"
end function

class enviroment
	public env
	private sub Class_Initialize()
		set env = CreateObject("Scripting.Dictionary")
		env.add "+",getref("add")
		env.add "-",getref("subtract")
		env.add "*",getref("multiply")
		env.add "/",getref("divide")
		env.add "donothing", getref("donothing")
	end sub

end class

Function READ(str)
    set READ = read_str(str)
End Function

Function EVAL(oMal,env)
	'msgbox typename(o)
	if isempty(o) then
		set EVAL = donothing("")
		exit function
	end if
	select case oMal.type_
		case "list()"
			if oMal.value_.count = 0 then
				set EVAL = oMal
			else
				'wsh.echo oMal.value_.item(0).value_
				'wsh.echo typename(env.env)
				'msgbox eval_ast(oMal.value_.item(1),env).value_
				'msgbox typename(env.env.item("+")(oMal))
				'if not isempty(oMal.value_.item(0)) then
					set EVAL = env.env.item(eval_ast(oMal.value_.item(0),env).value_)(eval_ast(oMal,env))
				'else
				'end if
			end if
		case else 
			set EVAL = eval_ast(oMal,env)
	end select
End Function

function eval_ast(ast,env)
	select case ast.type_
		case "list()"
			for i = 0 to ast.value_.count - 1
				set ast.value_.item(i) = EVAL(ast.value_.item(i),env)
			next
			set eval_ast = ast
		case "symbol"
			if env.env.Exists(ast.value_) then
				set eval_ast = ast
			else
				'err.raise vbObjectError, "eval_ast", "undefined symbol: " & ast.value_
				wsh.echo "undefined symbol: " & ast.value_
				ast.value_ = "donothing"
				set eval_ast = ast
			end if
		case "list[]"
			for i = 0 to ast.value_.count - 1
				set ast.value_.item(i) = EVAL(ast.value_.item(i),env)
			next
			set eval_ast = ast
		case "hash-map"
			For i = 0 To ast.value_.Count -1 ' 迭代数组。
				' wsh.echo ast.value_.keys()(i).value_
				' wsh.echo ast.value_.item(ast.value_.keys()(i)).value_
				set ast.value_.item(ast.value_.keys()(i)) = EVAL(ast.value_.item(ast.value_.keys()(i)),env)
			Next
			set eval_ast = ast
		case else
			set eval_ast = ast
	end select
end function


Function PRINT(oMal)
    PRINT = pr_str(oMal,true)
End Function

Function rep(str,env)
	'on error resume next
    rep = PRINT(EVAL(READ(str),env))
	'msgbox 2
	if err.number <> 0 then rep = err.description
	on error goto 0
End Function

While True
    WScript.StdOut.Write("user> ")
    code = WScript.StdIn.ReadLine()
	set env = new enviroment
    WScript.Echo(rep(code,env))
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