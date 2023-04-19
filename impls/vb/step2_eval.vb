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

Namespace Mal
    Class step2_eval
        ' read
        Shared Function READ(str As String) As MalVal
            Return reader.read_str(str)
        End Function

        ' eval
        Shared Function EVAL(orig_ast As MalVal, env As Dictionary(Of String, MalVal)) As MalVal

            'Console.WriteLine("EVAL: {0}", printer._pr_str(orig_ast, true))

            If TypeOf orig_ast Is MalSymbol Then
                Dim sym As MalSymbol = DirectCast(orig_ast, MalSymbol)
                return env.Item(sym.getName())
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
            Dim el As MalList = New MalList
            Dim mv As MalVal
            For Each mv In ast.getValue()
                el.conj_BANG(EVAL(mv, env))
            Next
            Dim f As MalFunc = DirectCast(el(0), MalFunc)
            Return f.apply(el.rest())
        End Function
        
        ' print
        Shared Function PRINT(exp As MalVal) As String
            return printer._pr_str(exp, TRUE)
        End Function

        ' repl
        Shared repl_env As Dictionary(Of String, MalVal)

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

            repl_env = New Dictionary(Of String, MalVal)
            repl_env.Add("+", New MalFunc(AddressOf add))
            repl_env.Add("-", New MalFunc(AddressOf minus))
            repl_env.Add("*", New MalFunc(AddressOf mult))
            repl_env.Add("/", New MalFunc(AddressOf div))


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
