Imports System
Imports System.IO
Imports System.Collections.Generic
Imports Mal
Imports MalVal = Mal.types.MalVal
Imports MalInt = Mal.types.MalInt
Imports MalString = Mal.types.MalString
Imports MalSymbol = Mal.types.MalSymbol
Imports MalList = Mal.types.MalList
Imports MalVector = Mal.types.MalVector
Imports MalHashMap = Mal.types.MalHashMap
Imports MalFunc = Mal.types.MalFunc
Imports MalEnv = Mal.env.Env

Namespace Mal
    Class stepA_mal
        ' read
        Shared Function READ(str As String) As MalVal
            Return reader.read_str(str)
        End Function

        ' eval
        Shared Function starts_with(ast As Malval, sym As String) As MalVal
            If ast.list_Q() Then
                Const lst As MalList = DirectCast(ast, MalList)
                If 0 < lst.size() Then
                    Const fst As MalSymbol = TryCast(lst(0), MalSymbol)
                    If fst IsNot Nothing AndAlso fst.getName() = sym Then
                        return lst(1)
                    End If
                End If
            End If
            return Nothing
        End Function

        Shared Function quasiquote(ast As MalVal) As MalVal
            If TypeOf ast Is Mal.types.MalSymbol or Typeof ast Is Mal.types.MalHashMap Then
                return New MalList(New MalSymbol("quote"), ast)
            End If
            Const source As MalList = TryCast(ast, MalList)
            If source Is Nothing Then
                return ast
            End If
            Const unquoted As MalVal = starts_with(ast, "unquote")
            If unquoted IsNot Nothing Then
                return unquoted
            End If
            Dim result As MalList = New MalList()
            For i As Integer = source.size()-1 To 0 Step -1
                Const elt As MalVal = source(i)
                Const splice_unquoted As MalVal = starts_with(elt, "splice-unquote")
                If splice_unquoted IsNot Nothing Then
                    result = New MalList(New MalSymbol("concat"), splice_unquoted, result)
                Else
                    result = New MalList(New MalSymbol("cons"), quasiquote(elt), result)
                End If
            Next
            If TypeOf ast Is MalVector Then
                result = New MalList(New MalSymbol("vec"), result)
            End If
            return result
        End Function

        ' TODO: move to types.vb when it is ported
        Class FClosure
            Public ast As MalVal
            Public params As MalList
            Public env As MalEnv
            Function fn(args as MalList) As MalVal
                return EVAL(ast, new MalEnv(env, params, args))
            End Function
        End Class

        Shared Function EVAL(orig_ast As MalVal, env As MalEnv) As MalVal
            Do

            Dim dbgeval As MalVal = env.do_get("DEBUG-EVAL")
            If dbgeval IsNot Nothing and dbgeval IsNot Mal.types.Nil and dbgeval IsNot Mal.types.MalFalse Then
                Console.WriteLine("EVAL: {0}", printer._pr_str(orig_ast, true))
            End If

            If TypeOf orig_ast Is MalSymbol Then
                Dim key As String = DirectCast(orig_ast, MalSymbol).getName()
                Dim result As MalVal = env.do_get(key)
                If result Is Nothing Then
                    throw New Mal.types.MalException("'" & key & "' not found")
                End If
                return result
            Else If TypeOf orig_ast Is MalVector Then
                Dim old_lst As MalList = DirectCast(orig_ast, MalList)
                Dim new_lst As MalList
                    new_lst = DirectCast(New MalVector, MalList)
                Dim mv As MalVal
                For Each mv in old_lst.getValue()
                    new_lst.conj_BANG(EVAL(mv, env))
                Next
                return new_lst
            Else If TypeOf orig_ast Is MalHashMap Then
                Dim new_dict As New Dictionary(Of String, MalVal)
                Dim entry As KeyValuePair(Of String, MalVal)
                For Each entry in DirectCast(orig_ast,MalHashMap).getValue()
                    new_dict.Add(entry.Key, EVAL(DirectCast(entry.Value,MalVal), env))
                Next
                return New MalHashMap(new_dict)
            Else If not orig_ast.list_Q() Then
                return orig_ast
            End If

            ' apply list
            Dim ast As MalList = DirectCast(orig_ast, MalList)

            If ast.size() = 0  Then
                return ast
            End If
            Dim a0 As MalVal = ast(0)
            Dim a0sym As String
            If TypeOf a0 is MalSymbol Then
                a0sym = DirectCast(a0,MalSymbol).getName()
            Else
                a0sym = "__<*fn*>__"
            End If

            Select a0sym
            Case "def!"
                Dim a1 As MalVal = ast(1)
                Dim a2 As MalVal = ast(2)
                Dim res As MalVal = EVAL(a2, env)
                env.do_set(DirectCast(a1,MalSymbol), res)
                return res
            Case "let*"
                Dim a1 As MalVal = ast(1)
                Dim a2 As MalVal = ast(2)
                Dim key As MalSymbol
                Dim val as MalVal
                Dim let_env As new MalEnv(env)
                For i As Integer = 0 To (DirectCast(a1,MalList)).size()-1 Step 2
                    key = DirectCast(DirectCast(a1,MalList)(i),MalSymbol)
                    val = DirectCast(a1,MalList)(i+1)
                    let_env.do_set(key, EVAL(val, let_env))
                Next
                orig_ast = a2
                env = let_env
            Case "quote"
                return ast(1)
            Case "quasiquote"
                orig_ast = quasiquote(ast(1))
            Case "defmacro!"
                Dim a1 As MalVal = ast(1)
                Dim a2 As MalVal = ast(2)
                Dim res As MalVal = DirectCast(EVAL(a2, env), MalFunc).asMacro()
                env.do_set(DirectCast(a1,MalSymbol), res)
                return res
            Case "try*"
                Try
                    return EVAL(ast(1), env)
                Catch e As Exception
                    If ast.size() > 2 Then
                        Dim exc As MalVal
                        Dim a2 As MalVal = ast(2)
                        Dim a20 As MalVal = DirectCast(a2,MalList)(0)
                        If DirectCast(a20,MalSymbol).getName() = "catch*" Then
                            If TypeOf e Is Mal.types.MalException Then
                                exc = DirectCast(e,Mal.types.MalException).getValue()
                            Else
                                exc = New MalString(e.Message)
                            End If
                            return EVAL(
                                DirectCast(a2,MalList)(2),
                                New MalEnv(env,
                                           DirectCast(a2,MalList).slice(1,2),
                                           New MalList(exc)))
                        End If
                    End If
                    Throw e
                End Try
            Case "do"
                For i As Integer = 1 To ast.size()-2
                    EVAL(ast(i), env)
                Next
                orig_ast = ast(ast.size()-1)
            Case "if"
                Dim a1 As MalVal = ast(1)
                Dim cond As MalVal = EVAL(a1, env)
                If cond Is Mal.types.Nil or cond Is Mal.types.MalFalse Then
                    ' eval false slot form
                    If ast.size() > 3 Then
                        orig_ast = ast(3)
                    Else
                        return Mal.types.Nil
                    End If
                Else
                    ' eval true slot form
                    orig_ast = ast(2)

                End If
            Case "fn*"
                Dim fc As New FClosure()
                fc.ast = ast(2)
                fc.params = DirectCast(ast(1),MalLIst)
                fc.env = env
                Dim f As Func(Of MalList, MalVal) = AddressOf fc.fn
                Dim mf As new MalFunc(ast(2), env,
                                      DirectCast(ast(1),MalList), f)
                return DirectCast(mf,MalVal)
            Case Else
                Dim f As MalFunc = DirectCast(EVAL(a0, env), MalFunc)
                If f.isMacro() Then
                    orig_ast = f.apply(ast.rest())
                    Continue Do
                End If
                Dim args As MalList = New MalList
                For i As Integer = 1 To ast.size()-1
                    args.conj_BANG(EVAL(ast(i), env))
                Next
                Dim fnast As MalVal = f.getAst()
                If not fnast Is Nothing
                    orig_ast = fnast
                    env = f.genEnv(args)
                Else
                    Return f.apply(args)
                End If
            End Select

            Loop While True
        End Function
        
        ' print
        Shared Function PRINT(exp As MalVal) As String
            return printer._pr_str(exp, TRUE)
        End Function

        ' repl
        Shared repl_env As MalEnv

        Shared Function REP(str As String) As String
            Return PRINT(EVAL(READ(str), repl_env))
        End Function

        Shared Function do_eval(args As MalList) As MalVal
            Return EVAL(args(0), repl_env)
        End Function

        Shared Function Main As Integer
            Dim args As String() = Environment.GetCommandLineArgs()

            repl_env = New MalEnv(Nothing)

            ' core.vb: defined using VB.NET
            For Each entry As KeyValuePair(Of String,MalVal) In core.ns()
                repl_env.do_set(new MalSymbol(entry.Key), entry.Value)
            Next
            repl_env.do_set(new MalSymbol("eval"), new MalFunc(AddressOf do_eval))
            Dim fileIdx As Integer = 1
            If args.Length > 1 AndAlso args(1) = "--raw" Then
                Mal.readline.SetMode(Mal.readline.Modes.Raw)
                fileIdx = 2
            End If
            Dim argv As New MalList()
            For i As Integer = fileIdx+1 To args.Length-1
                argv.conj_BANG(new MalString(args(i)))
            Next
            repl_env.do_set(new MalSymbol("*ARGV*"), argv)

            ' core.mal: defined using the language itself
            REP("(def! *host-language* ""VB.NET"")")
            REP("(def! not (fn* (a) (if a false true)))")
            REP("(def! load-file (fn* (f) (eval (read-string (str ""(do "" (slurp f) ""\nnil)"")))))")
            REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw ""odd number of forms to cond"")) (cons 'cond (rest (rest xs)))))))")

            If args.Length > fileIdx Then
                REP("(load-file """ & args(fileIdx) & """)")
                return 0
            End If

            ' repl loop
            Dim line As String
            REP("(println (str ""Mal ["" *host-language* ""]""))")
            Do
                Try
                    line = Mal.readline.Readline("user> ")
                    If line is Nothing Then
                        Exit Do
                    End If
                    If line = "" Then
                        Continue Do
                    End If
                Catch e As IOException
                    Console.WriteLine("IOException: " & e.Message)
                End Try
                Try
                    Console.WriteLine(REP(line))
                Catch e As Mal.types.MalException
                    Console.WriteLine("Error: " & _
                        printer._pr_str(e.getValue(), False))
                    Continue Do
                Catch e As Exception
                    Console.WriteLine("Error: " & e.Message)
                    Console.WriteLine(e.StackTrace)
                    Continue Do
                End Try
            Loop While True
        End function
    End Class
End Namespace
