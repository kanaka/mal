Option Explicit

Include "IO.vbs"
Include "Types.vbs"
Include "Reader.vbs"
Include "Printer.vbs"
Include "Env.vbs"
Include "Core.vbs"

Class TailCall
	Public objMalType
	Public objEnv
End Class

Function EvalLater(objMal, objEnv)
	Dim varRes
	Set varRes = New TailCall
	Set varRes.objMalType = objMal
	Set varRes.objEnv = objEnv
	Set EvalLater = varRes
End Function

Function MDef(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.SYMBOL
	Set varRet = Evaluate(objArgs.Item(2), objEnv)
	objEnv.Add objArgs.Item(1), varRet
	Set MDef = varRet
End Function
objNS.Add NewMalSym("def!"), NewVbsProc("MDef", True)

Function MLet(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2

	Dim objBinds
	Set objBinds = objArgs.Item(1)
	CheckListOrVec objBinds
	
	If objBinds.Count Mod 2 <> 0 Then
		Err.Raise vbObjectError, _
			"MLet", "Wrong argument count."
	End If

	Dim objNewEnv
	Set objNewEnv = NewEnv(objEnv)
	Dim i, objSym
	For i = 0 To objBinds.Count - 1 Step 2
		Set objSym = objBinds.Item(i)
		CheckType objSym, TYPES.SYMBOL
		objNewEnv.Add objSym, Evaluate(objBinds.Item(i + 1), objNewEnv)
	Next

	Set varRet = EvalLater(objArgs.Item(2), objNewEnv)
	Set MLet = varRet
End Function
objNS.Add NewMalSym("let*"), NewVbsProc("MLet", True)

Function MDo(objArgs, objEnv)
	Dim varRet, i
	If objArgs.Count - 1 < 1 Then
		Err.Raise vbObjectError, _
			"MDo", "Need more arguments."
	End If
	For i = 1 To objArgs.Count - 2
		Call Evaluate(objArgs.Item(i), objEnv)
	Next
	Set varRet = EvalLater( _
		objArgs.Item(objArgs.Count - 1), _
		objEnv)
	Set MDo = varRet
End Function
objNS.Add NewMalSym("do"), NewVbsProc("MDo", True)

Function MIf(objArgs, objEnv)
	Dim varRet
	If objArgs.Count - 1 <> 3 And _
		objArgs.Count - 1 <> 2 Then
		Err.Raise vbObjectError, _
			"MIf", "Wrong number of arguments."
	End If

	Dim objCond
	Set objCond = Evaluate(objArgs.Item(1), objEnv)
	Dim boolCond
	If objCond.Type = TYPES.BOOLEAN Then
		boolCond = objCond.Value
	Else
		boolCond = True
	End If
	boolCond = (boolCond And objCond.Type <> TYPES.NIL)
	If boolCond Then
		Set varRet = EvalLater(objArgs.Item(2), objEnv)
	Else
		If objArgs.Count - 1 = 3 Then
			Set varRet = EvalLater(objArgs.Item(3), objEnv)
		Else
			Set varRet = NewMalNil()
		End If
	End If
	Set MIf = varRet
End Function
objNS.Add NewMalSym("if"), NewVbsProc("MIf", True)

Function MFn(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2

	Dim objParams, objCode
	Set objParams = objArgs.Item(1)
	CheckListOrVec objParams
	Set objCode = objArgs.Item(2)
	
	Dim i
	For i = 0 To objParams.Count - 1
		CheckType objParams.Item(i), TYPES.SYMBOL
	Next
	Set varRet = NewMalProc(objParams, objCode, objEnv)
	Set MFn = varRet
End Function
objNS.Add NewMalSym("fn*"), NewVbsProc("MFn", True)

Function MEval(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = Evaluate(objArgs.Item(1), objEnv)
	Set varRes = EvalLater(varRes, objNS)
	Set MEval = varRes
End Function
objNS.Add NewMalSym("eval"), NewVbsProc("MEval", True)

Function MQuote(objArgs, objEnv)
	CheckArgNum objArgs, 1
	Set MQuote = objArgs.Item(1)
End Function
objNS.Add NewMalSym("quote"), NewVbsProc("MQuote", True)

Function MQuasiQuote(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	
	Set varRes = EvalLater( _
		MQuasiQuoteExpand(objArgs, objEnv), objEnv)
	Set MQuasiQuote = varRes
End Function
objNS.Add NewMalSym("quasiquote"), NewVbsProc("MQuasiQuote", True)

Function MQuasiQuoteExpand(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1

	Set varRes = ExpandHelper(objArgs.Item(1))
	If varRes.Splice Then
		Err.Raise vbObjectError, _
			"MQuasiQuoteExpand", "Wrong return value type."
	End If
	Set varRes = varRes.Value

	Set MQuasiQuoteExpand = varRes
End Function
objNS.Add NewMalSym("quasiquoteexpand"), NewVbsProc("MQuasiQuoteExpand", True)

Class ExpandType
	Public Splice
	Public Value
End Class

Function NewExpandType(objValue, boolSplice)
	Dim varRes
	Set varRes = New ExpandType
	Set varRes.Value = objValue
	varRes.Splice = boolSplice
	Set NewExpandType = varRes
End Function

Function ExpandHelper(objArg)
	Dim varRes, boolSplice
	Dim varBuilder, varEType, i
	boolSplice = False
	Select Case objArg.Type
		Case TYPES.LIST
			Dim boolNormal
			boolNormal = False

			' Check for unquotes.
			Select Case objArg.Count
				Case 2
					' Maybe have a bug here
					' like (unquote a b c) should be throw a error
					If objArg.Item(0).Type = TYPES.SYMBOL Then
						Select Case objArg.Item(0).Value
							Case "unquote"
								Set varRes = objArg.Item(1)
							Case "splice-unquote"
								Set varRes = objArg.Item(1)
								boolSplice = True
							Case Else
								boolNormal = True
						End Select
					Else
						boolNormal = True
					End If
				Case Else
					boolNormal = True
			End Select
			
			If boolNormal Then
				Set varRes = NewMalList(Array())
				Set varBuilder = varRes

				For i = 0 To objArg.Count - 1
					Set varEType = ExpandHelper(objArg.Item(i))
					If varEType.Splice Then
						varBuilder.Add NewMalSym("concat")
					Else
						varBuilder.Add NewMalSym("cons")
					End If
					varBuilder.Add varEType.Value
					varBuilder.Add NewMalList(Array())
					Set varBuilder = varBuilder.Item(2)
				Next
			End If
		Case TYPES.VECTOR
			Set varRes = NewMalList(Array( _
				NewMalSym("vec"), NewMalList(Array())))
			
			Set varBuilder = varRes.Item(1)
			For i = 0 To objArg.Count - 1
				Set varEType = ExpandHelper(objArg.Item(i))
				If varEType.Splice Then
					varBuilder.Add NewMalSym("concat")
				Else
					varBuilder.Add NewMalSym("cons")
				End If
				varBuilder.Add varEType.Value
				varBuilder.Add NewMalList(Array())
				Set varBuilder = varBuilder.Item(2)
			Next
		Case TYPES.HASHMAP
			' Maybe have a bug here.
			' e.g. {"key" ~value}
			Set varRes = NewMalList(Array( _
				NewMalSym("quote"), objArg))
		Case TYPES.SYMBOL
			Set varRes = NewMalList(Array( _
				NewMalSym("quote"), objArg))
		Case Else
			' Maybe have a bug here.
			' All unspecified type will return itself.
			Set varRes = objArg
	End Select

	Set ExpandHelper = NewExpandType(varRes, boolSplice)
End Function

Function MDefMacro(objArgs, objEnv)
	Dim varRet
	CheckArgNum objArgs, 2
	CheckType objArgs.Item(1), TYPES.SYMBOL
	Set varRet = Evaluate(objArgs.Item(2), objEnv).Copy()
	CheckType varRet, TYPES.PROCEDURE
	varRet.IsMacro = True
	objEnv.Add objArgs.Item(1), varRet
	Set MDefMacro = varRet
End Function
objNS.Add NewMalSym("defmacro!"), NewVbsProc("MDefMacro", True)

Function IsMacroCall(objCode, objEnv)
	Dim varRes
	varRes = False

	' VBS has no short-circuit evaluation.
	If objCode.Type = TYPES.LIST Then
		If objCode.Count > 0 Then
			If objCode.Item(0).Type = TYPES.SYMBOL Then
				Dim varValue
				Set varValue = objEnv.Get(objCode.Item(0))
				If varValue.Type = TYPES.PROCEDURE Then
					If varValue.IsMacro Then
						varRes = True
					End If
				End If
			End If
		End If
	End If

	IsMacroCall = varRes
End Function

Function MacroExpand(ByVal objAST, ByVal objEnv)
	Dim varRes
	While IsMacroCall(objAST, objEnv)
		Dim varMacro
		Set varMacro = objEnv.Get(objAST.Item(0))
		Set objAST = varMacro.MacroApply(objAST, objEnv)		
	Wend
	Set varRes = objAST
	Set MacroExpand = varRes
End Function

Function MMacroExpand(objArgs, objEnv)
	Dim varRes
	CheckArgNum objArgs, 1
	Set varRes = MacroExpand(objArgs.Item(1), objEnv)
	Set MMacroExpand = varRes
End Function
objNS.Add NewMalSym("macroexpand"), NewVbsProc("MMacroExpand", True)

Function MTry(objArgs, objEnv)
	Dim varRes
	
	If objArgs.Count - 1 < 1 Then
		Err.Raise vbObjectError, _
			"MTry", "Need more arguments."
	End If

	If objArgs.Count - 1 = 1 Then
		Set varRes = EvalLater(objArgs.Item(1), objEnv)
		Set MTry = varRes
		Exit Function
	End If

	CheckArgNum objArgs, 2
	CheckType objArgs.Item(2), TYPES.LIST

	Dim objTry, objCatch
	Set objTry = objArgs.Item(1)
	Set objCatch = objArgs.Item(2)
	
	CheckArgNum objCatch, 2
	CheckType objCatch.Item(0), TYPES.SYMBOL
	CheckType objCatch.Item(1), TYPES.SYMBOL
	If objCatch.Item(0).Value <> "catch*" Then
		Err.Raise vbObjectError, _
			"MTry", "Unexpect argument(s)."
	End If
	
	On Error Resume Next
	Set varRes = Evaluate(objTry, objEnv)
	If Err.Number <> 0 Then
		Dim objException

		If Err.Source <> "MThrow" Then
			Set objException = NewMalStr(Err.Description)
		Else
			Set objException = objExceptions.Item(Err.Description)
			objExceptions.Remove Err.Description
		End If
		
		Call Err.Clear()
		On Error Goto 0

		' The code below may cause error too.
		' So we should clear err info & throw out any errors.
		' Use 'quote' to avoid eval objExp again.
		Set varRes = Evaluate(NewMalList(Array( _
			NewMalSym("let*"), NewMalList(Array( _
				objCatch.Item(1), NewMalList(Array( _
						NewMalSym("quote"), objException)))), _
			objCatch.Item(2))), objEnv)
	Else
		On Error Goto 0
	End If

	Set MTry = varRes
End Function
objNS.Add NewMalSym("try*"), NewVbsProc("MTry", True)

Call InitBuiltIn()
Call InitMacro()

Call InitArgs()
Sub InitArgs()
	Dim objArgs
	Set objArgs = NewMalList(Array())

	Dim i
	For i = 1 To WScript.Arguments.Count - 1
		objArgs.Add NewMalStr(WScript.Arguments.Item(i))
	Next
	
	objNS.Add NewMalSym("*ARGV*"), objArgs
	
	If WScript.Arguments.Count > 0 Then
		REP "(load-file """ + WScript.Arguments.Item(0) + """)"
		WScript.Quit 0
	End If
End Sub

Call REPL()
Sub REPL()
	Dim strCode
	While True
		IO.Write "user> "

		On Error Resume Next
			strCode = IO.ReadLine
			If Err.Number <> 0 Then WScript.Quit 0
		On Error Goto 0

		Dim strRes
		On Error Resume Next
			strRes = REP(strCode)
			If Err.Number <> 0 Then
				If Err.Source = "MThrow" Then
					IO.WriteErrLine "Exception: " + _
						PrintMalType(objExceptions.Item(Err.Description), True)
					objExceptions.Remove Err.Description
				Else
					IO.WriteErrLine "Exception: " + Err.Description
				End If
			Else
				If strRes <> "" Then
					IO.WriteLine strRes
				End If
			End If
		On Error Goto 0
	Wend
End Sub

Function Read(strCode)
	Set Read = ReadString(strCode)
End Function

Function Evaluate(ByVal objCode, ByVal objEnv)
	While True
		If TypeName(objCode) = "Nothing" Then
			Set Evaluate = Nothing
			Exit Function
		End If
		
		Set objCode = MacroExpand(objCode, objEnv)

		Dim varRet, objFirst
		If objCode.Type = TYPES.LIST Then
			If objCode.Count = 0 Then ' ()
				Set Evaluate = objCode
				Exit Function
			End If

			Set objFirst = Evaluate(objCode.Item(0), objEnv)
			Set varRet = objFirst.Apply(objCode, objEnv)
		Else
			Set varRet = EvaluateAST(objCode, objEnv)
		End If
		
		If TypeName(varRet) = "TailCall" Then
			' NOTICE: If not specify 'ByVal', 
			' Change of arguments will influence
			' the caller's variable!
			Set objCode = varRet.objMalType
			Set objEnv = varRet.objEnv
		Else
			Set Evaluate = varRet
			Exit Function
		End If
	Wend
End Function


Function EvaluateAST(objCode, objEnv)
	Dim varRet, i
	Select Case objCode.Type
		Case TYPES.SYMBOL
			Set varRet = objEnv.Get(objCode)
		Case TYPES.LIST
			Err.Raise vbObjectError, _
				"EvaluateAST", "Unexpect type."
		Case TYPES.VECTOR
			Set varRet = NewMalVec(Array())
			For i = 0 To objCode.Count() - 1
				varRet.Add Evaluate(objCode.Item(i), objEnv)
			Next
		Case TYPES.HASHMAP
			Set varRet = NewMalMap(Array(), Array())
			For Each i In objCode.Keys()
				varRet.Add i, Evaluate(objCode.Item(i), objEnv)
			Next
		Case Else
			Set varRet = objCode
	End Select
	Set EvaluateAST = varRet
End Function

Function EvaluateRest(objCode, objEnv)
	Dim varRet, i
	Select Case objCode.Type
		Case TYPES.LIST
			Set varRet = NewMalList(Array(NewMalNil()))
			For i = 1 To objCode.Count() - 1
				varRet.Add Evaluate(objCode.Item(i), objEnv)
			Next
		Case Else
			Err.Raise vbObjectError, _
				"EvaluateRest", "Unexpected type."
	End Select
	Set EvaluateRest = varRet
End Function

Function Print(objCode)
	Print = PrintMalType(objCode, True)
End Function

Function REP(strCode)
	REP = Print(Evaluate(Read(strCode), objNS))
End Function

Sub Include(strFileName)
	With CreateObject("Scripting.FileSystemObject")
		ExecuteGlobal .OpenTextFile( _
			.GetParentFolderName( _
			.GetFile(WScript.ScriptFullName)) & _
			"\" & strFileName).ReadAll
	End With
End Sub