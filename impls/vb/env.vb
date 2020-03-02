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
            
            Public Function find(key As MalSymbol) As Env
                If data.ContainsKey(key.getName()) Then
                    return Me
                Else If outer IsNot Nothing Then
                    return outer.find(key)
                Else
                    return Nothing
                End If
            End Function

            Public Function do_get(key As MalSymbol) As MalVal
                Dim e As Env = find(key)
                If e Is Nothing Then
                    throw New Mal.types.MalException(
                            "'" & key.getName() & "' not found")
                Else
                    return e.data(key.getName())
                End If
            End Function

            Public Function do_set(key As MalSymbol, value As MalVal) As Env
                data(key.getName()) = value
                return Me
            End Function
        End Class
    End Class
End Namespace
