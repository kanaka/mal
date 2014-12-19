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
    Class step9_try
        ' read
        Shared Function READ(str As String) As MalVal
            Return reader.read_str(str)
        End Function

        ' eval
        Shared Function is_pair(x As MalVal) As Boolean
            return TypeOf x Is MalList AndAlso _
                   DirectCast(x,MalList).size() > 0
        End Function

        Shared Function quasiquote(ast As MalVal) As MalVal
            If not is_pair(ast) Then
                return New MalList(New MalSymbol("quote"), ast)
            Else
                Dim a0 As MalVal = DirectCast(ast,MalList)(0)
                If TypeOf a0 Is MalSymbol AndAlso _
                    DirectCast(a0,MalSymbol).getName() = "unquote" Then
                    return DirectCast(ast,MalList)(1)
                Else If is_pair(a0) Then
                    Dim a00 As MalVal = DirectCast(a0,MalList)(0)
                    If TypeOf a00 is MalSymbol AndAlso _
                        DirectCast(a00,MalSymbol).getName() = "splice-unquote" Then
                        return New MalList(New MalSymbol("concat"),
                                           DirectCast(a0,MalList)(1),
                                           quasiquote(DirectCast(ast,MalList).rest()))
                    End If
                End If
                return New MalList(New MalSymbol("cons"),
                                   quasiquote(a0),
                                   quasiquote(DirectCast(ast,MalList).rest()))
            End If
        End Function

        Shared Function is_macro_call(ast As MalVal, env As MalEnv) As Boolean
            If TypeOf ast Is MalList Then
                Dim a0 As MalVal = DirectCast(ast,MalList)(0)
                If TypeOf a0 Is MalSymbol AndAlso _
                    env.find(DirectCast(a0,MalSymbol)) IsNot Nothing Then
                    Dim mac As MalVal = env.do_get(DirectCast(a0,MalSymbol))
                    If TypeOf mac Is MalFunc AndAlso _
                        DirectCast(mac,MalFunc).isMacro() Then
                        return True
                    End If
                End If
            End If
            return False
        End Function

        Shared Function macroexpand(ast As MalVal, env As MalEnv) As MalVal
            While is_macro_call(ast, env)
                Dim a0 As MalSymbol = DirectCast(DirectCast(ast,MalList)(0),MalSymbol)
                Dim mac As MalFunc = DirectCast(env.do_get(a0),MalFunc)
                ast = mac.apply(DirectCast(ast,MalList).rest())
            End While
            return ast
        End Function

        Shared Function eval_ast(ast As MalVal, env As MalEnv) As MalVal
            If TypeOf ast Is MalSymbol Then
                return env.do_get(DirectCast(ast, MalSymbol))
            Else If TypeOf ast Is MalList Then
                Dim old_lst As MalList = DirectCast(ast, MalList)
                Dim new_lst As MalList
                If ast.list_Q() Then
                    new_lst = New MalList
                Else
                    new_lst = DirectCast(New MalVector, MalList)
                End If
                Dim mv As MalVal
                For Each mv in old_lst.getValue()
                    new_lst.conj_BANG(EVAL(mv, env))
                Next
                return new_lst
            Else If TypeOf ast Is MalHashMap Then
                Dim new_dict As New Dictionary(Of String, MalVal)
                Dim entry As KeyValuePair(Of String, MalVal)
                For Each entry in DirectCast(ast,MalHashMap).getValue()
                    new_dict.Add(entry.Key, EVAL(DirectCast(entry.Value,MalVal), env))
                Next
                return New MalHashMap(new_dict)
            Else
                return ast
            End If
            return ast
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

            'Console.WriteLine("EVAL: {0}", printer._pr_str(orig_ast, true))
            If not orig_ast.list_Q() Then
                return eval_ast(orig_ast, env)
            End If

            ' apply list
            Dim expanded As MalVal = macroexpand(orig_ast, env)
            if not expanded.list_Q() Then
                return expanded
            End If
            Dim ast As MalList = DirectCast(expanded, MalList)

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
                Dim res As MalVal = EVAL(a2, env)
                DirectCast(res,MalFunc).setMacro()
                env.do_set(DirectCast(a1,MalSymbol), res)
                return res
            Case "macroexpand"
                Dim a1 As MalVal = ast(1)
                return macroexpand(a1, env)
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
                                exc = New MalString(e.StackTrace)
                            End If
                            return EVAL(
                                DirectCast(a2,MalList)(2),
                                New MalEnv(env,
                                           DirectCast(a2,MalList).slice(1,2),
                                           New MalList(exc)))
                        End If
                        Throw e
                    End If
                End Try
            Case "do"
                eval_ast(ast.slice(1, ast.size()-1), env)
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
                Dim el As MalList = DirectCast(eval_ast(ast, env), MalList)
                Dim f As MalFunc = DirectCast(el(0), MalFunc)
                Dim fnast As MalVal = f.getAst()
                If not fnast Is Nothing
                    orig_ast = fnast
                    env = f.genEnv(el.rest())
                Else
                    Return f.apply(el.rest())
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
            REP("(def! not (fn* (a) (if a false true)))")
            REP("(def! load-file (fn* (f) (eval (read-string (str ""(do "" (slurp f) "")"")))))")
            REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw ""odd number of forms to cond"")) (cons 'cond (rest (rest xs)))))))")
            REP("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

            If args.Length > fileIdx Then
                REP("(load-file """ & args(fileIdx) & """)")
                return 0
            End If

            ' repl loop
            Dim line As String
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
