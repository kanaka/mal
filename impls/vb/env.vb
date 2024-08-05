Imports System.Collections.Generic
Imports Mal
Imports MalVal = Mal.types.MalVal
Imports MalSymbol = Mal.types.MalSymbol
Imports MalList = Mal.types.MalList

Namespace Mal
    Public Class env
        Public Class Env
            Dim outer As Env = Nothing
            Dim data As Dictionary(Of String, MalVal) = New Dictionary(Of String, MalVal)

            Public Sub New(new_outer As Env)
                outer = new_outer
            End Sub
            Public Sub New(new_outer As Env, binds As MalList, exprs As MalList)
                outer = new_outer
                For i As Integer = 0 To binds.size()-1
                    Dim sym As String = DirectCast(binds.nth(i),MalSymbol).getName()
                    If sym = "&" Then
                        data(DirectCast(binds.nth(i+1),MalSymbol).getName()) = exprs.slice(i)
                        Exit For
                    Else
                        data(sym) = exprs.nth(i)
                    End If
                Next
            End Sub
            
            Public Function do_get(key As String) As MalVal
                If data.ContainsKey(key) Then
                    return data(key)
                Else If outer IsNot Nothing Then
                    return outer.do_get(key)
                Else
                    return Nothing
                End If
            End Function

            Public Function do_set(key As MalSymbol, value As MalVal) As Env
                data(key.getName()) = value
                return Me
            End Function
        End Class
    End Class
End Namespace
