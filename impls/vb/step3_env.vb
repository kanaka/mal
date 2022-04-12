Imports System
Imports System.IO
Imports System.Collections.Generic
Imports Mal
Imports MalVal = Mal.types.MalVal
Imports MalInt = Mal.types.MalInt
Imports MalSymbol = Mal.types.MalSymbol
Imports MalList = Mal.types.MalList
Imports MalVector = Mal.types.MalVector
Imports MalHashMap = Mal.types.MalHashMap
Imports MalFunc = Mal.types.MalFunc
Imports MalEnv = Mal.env.Env

Namespace Mal
    Class step3_env
        ' read
        Shared Function READ(str As String) As MalVal
            Return reader.read_str(str)
        End Function

        ' eval
        Shared Function EVAL(orig_ast As MalVal, env As MalEnv) As MalVal

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
            Select DirectCast(a0,MalSymbol).getName()
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
                return EVAL(a2, let_env)
            Case Else
                Dim el As MalList = New MalList
                Dim mv As MalVal
                For Each mv In ast.getValue()
                    el.conj_BANG(EVAL(mv, env))
                Next
                Dim f As MalFunc = DirectCast(el(0), MalFunc)
                Return f.apply(el.rest())
            End Select
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

        Shared Function add(a As MalList) As MalVal
            Return DirectCast(a.Item(0),MalInt) + DirectCast(a.Item(1),MalInt)
        End Function

        Shared Function minus(a As MalList) As MalVal
            Return DirectCast(a.Item(0),MalInt) - DirectCast(a.Item(1),MalInt)
        End Function

        Shared Function mult(a As MalList) As MalVal
            Return DirectCast(a.Item(0),MalInt) * DirectCast(a.Item(1),MalInt)
        End Function

        Shared Function div(a As MalList) As MalVal
            Return DirectCast(a.Item(0),MalInt) / DirectCast(a.Item(1),MalInt)
        End Function

        Shared Function Main As Integer
            Dim args As String() = Environment.GetCommandLineArgs()

            repl_env = New MalEnv(Nothing)
            repl_env.do_set(new MalSymbol("+"), New MalFunc(AddressOf add))
            repl_env.do_set(new MalSymbol("-"), New MalFunc(AddressOf minus))
            repl_env.do_set(new MalSymbol("*"), New MalFunc(AddressOf mult))
            repl_env.do_set(new MalSymbol("/"), New MalFunc(AddressOf div))


            If args.Length > 1 AndAlso args(1) = "--raw" Then
                Mal.readline.SetMode(Mal.readline.Modes.Raw)
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
                Catch e as Exception
                    Console.WriteLine("Error: " & e.Message)
                    Console.WriteLine(e.StackTrace)
                    Continue Do
                End Try
            Loop While True
        End function
    End Class
End Namespace
