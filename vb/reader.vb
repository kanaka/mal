Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Text.RegularExpressions
Imports Mal
Imports MalVal = Mal.types.MalVal
Imports MalSymbol = Mal.types.MalSymbol
Imports MalList = Mal.types.MalList
Imports MalVector = Mal.types.MalVector
Imports MalHashMap = Mal.types.MalHashMap
Imports MalThrowable = Mal.types.MalThrowable
Imports MalContinue = Mal.types.MalContinue

Namespace Mal
    Public Class reader
        Public Class ParseError
            Inherits MalThrowable
            Public Sub New(msg As String)
                MyBase.New(msg)
            End Sub
        End Class

        Public Class Reader
            Private tokens As New List(Of String)
            Private position As Int32 = 0
            Sub New(t As List(Of String))
                tokens = t
                position = 0
            End Sub

            Public Function peek() As String
                If position >= tokens.Count Then
                    return Nothing
                Else
                    return tokens(position)
                End If
            End Function

            Public Function get_next() As String
                If position >= tokens.Count Then
                    return Nothing
                Else
                    position += 1
                    return tokens(position-1)
                End If
            End Function
        End Class

        Shared Function tokenize(str As String) As List(Of String)
            Dim tokens As New List(Of String)
            Dim pattern As String = "[\s ,]*(~@|[\[\]{}()'`~@]|""(?:[\\].|[^\\""])*""|;.*|[^\s \[\]{}()'""`~@,;]*)"
            Dim regex As New Regex(pattern)
            For Each match As Match In regex.Matches(str)
                Dim token As String = match.Groups(1).Value
                If Not token Is Nothing _
                    AndAlso Not token = "" _
                    AndAlso Not token(0) = ";" Then
                    'Console.WriteLine("match: ^" & match.Groups[1] & "$")
                    tokens.Add(token)
                End If
            Next
            return tokens
        End Function

        Shared Function read_atom(rdr As Reader) As MalVal
            Dim token As String = rdr.get_next()
            Dim pattern As String = "(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^("".*"")$|^:(.*)|(^[^""]*$)"
            Dim regex As Regex = New Regex(pattern)
            Dim match As Match = regex.Match(token)
            'Console.WriteLine("token: ^" + token + "$")
            If not match.Success Then
                throw New ParseError("unrecognized token '" & token & "'")
            End If
            If match.Groups(1).Value <> String.Empty Then
                return New Mal.types.MalInt(Integer.Parse(match.Groups(1).Value))
            Else If match.Groups(3).Value <> String.Empty Then
                return Mal.types.Nil
            Else If match.Groups(4).Value <> String.Empty Then
                return Mal.types.MalTrue
            Else If match.Groups(5).Value <> String.Empty Then
                return Mal.types.MalFalse
            Else If match.Groups(6).Value <> String.Empty Then
                Dim str As String = match.Groups(6).Value
                return New Mal.types.MalString(
                        str.Substring(1, str.Length-2) _
                        .Replace("\""", """") _
                        .Replace("\n", Environment.NewLine) _
                        .Replace("\\", "\"))
            Else If match.Groups(7).Value <> String.Empty Then
                return New Mal.types.MalString(ChrW(&H029e) & match.Groups(7).Value)
            Else If match.Groups(8).Value <> String.Empty Then
                return New Mal.types.MalSymbol(match.Groups(8).Value)
            Else
                throw New ParseError("unrecognized '" & match.Groups(0).Value & "'")
            End If
        End Function

        Shared Function read_list(rdr As Reader, lst As MalList,
                                  start As String, last As String) As MalVal
            Dim token As String = rdr.get_next()
            If token(0) <> start Then
                throw New ParseError("expected '" & start & "'")
            End If
            
            token = rdr.peek()
            While token IsNot Nothing AndAlso token(0) <> last
                lst.conj_BANG(read_form(rdr))
                token = rdr.peek()
            End While

            If token Is Nothing Then
                throw New ParseError("expected '" & last & "', got EOF")
            End If
            rdr.get_next()

            return lst
        End Function

        Shared Function read_hash_map(rdr As Reader) As MalVal
            Dim lst As MalList = DirectCast(read_list(rdr, new MalList(),
                                                      "{", "}"),MalList)
            return New MalHashMap(lst)
        End Function


        Shared Function read_form(rdr As Reader) As MalVal
            Dim token As String = rdr.peek()
            If token Is Nothing Then
                throw New MalContinue()
            End If
            Dim form As MalVal = Nothing

            Select token
            Case "'"
                rdr.get_next()
                return New MalList(New MalSymbol("quote"),
                                   read_form(rdr))
            Case "`"
                rdr.get_next()
                return New MalList(New MalSymbol("quasiquote"),
                                   read_form(rdr))
            Case "~"
                rdr.get_next()
                return New MalList(New MalSymbol("unquote"),
                                   read_form(rdr))
            Case "~@"
                rdr.get_next()
                return new MalList(New MalSymbol("splice-unquote"),
                                   read_form(rdr))
            Case "^"
                rdr.get_next()
                Dim meta As MalVal = read_form(rdr)
                return new MalList(New MalSymbol("with-meta"),
                                   read_form(rdr),
                                   meta)
            Case "@"
                rdr.get_next()
                return new MalList(New MalSymbol("deref"),
                                   read_form(rdr))

            Case "("
                form = read_list(rdr, New MalList(), "(" , ")")
            Case ")"
                throw New ParseError("unexpected ')'")
            Case "["
                form = read_list(rdr, New MalVector(), "[" , "]")
            Case "]"
                throw New ParseError("unexpected ']'")
            Case "{"
                form = read_hash_map(rdr)
            Case "}"
                throw New ParseError("unexpected '}'")
            Case Else
                form = read_atom(rdr)
            End Select
            return form
        End Function


        Shared Function read_str(str As string) As MalVal
            return read_form(New Reader(tokenize(str)))
        End Function
    End Class
End Namespace
