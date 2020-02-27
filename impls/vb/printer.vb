Imports System
Imports System.Collections.Generic
Imports System.Text.RegularExpressions
Imports Mal
Imports MalVal = Mal.types.MalVal
Imports MalList = Mal.types.MalList

Namespace Mal
    Public Class printer
        Shared Function join(value As List(Of MalVal),
                             delim As String,
                             print_readably As Boolean) As String
            Dim strs As New List(Of String)
            For Each mv As MalVal In value
                strs.Add(mv.ToString(print_readably))
            Next
            return String.Join(delim, strs.ToArray())
        End Function

        Shared Function join(value As Dictionary(Of String, MalVal),
                             delim As String,
                             print_readably As Boolean) As String
            Dim strs As New List(Of String)
            For Each entry As KeyValuePair(Of String, MalVal) In value
                If entry.Key.Length > 0 and entry.Key(0) = ChrW(&H029e) Then
                    strs.Add(":" & entry.Key.Substring(1))
                Else If print_readably Then
                    strs.Add("""" & entry.Key.ToString() & """")
                Else
                    strs.Add(entry.Key.ToString())
                End If
                strs.Add(entry.Value.ToString(print_readably))
            Next
            return String.Join(delim, strs.ToArray())
        End Function

        Shared Function _pr_str(mv As MalVal,
                               print_readably As Boolean) As String
            return mv.ToString(print_readably)
        End Function

        Shared Function _pr_str_args(args As MalList,
                                     sep As String,
                                     print_readably As Boolean) As String
            return join(args.getValue(), sep, print_readably)
        End Function

        Shared Function escapeString(str As String) As String
            return Regex.Escape(str)
        End Function
    End Class
End Namespace
