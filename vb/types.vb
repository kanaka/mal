Imports System
Imports System.Collections.Generic
Imports System.Text.RegularExpressions
Imports Mal

namespace Mal
    Public Class types
        '
        ' Exceptions/Errors
        '
        Public Class MalThrowable
            Inherits Exception
            Public Sub New()
                MyBase.New()
            End Sub
            Public Sub New(msg As String)
                MyBase.New(msg)
            End Sub
        End Class
        Public Class MalError
            Inherits MalThrowable
            Public Sub New(msg As String)
                MyBase.New(msg)
            End Sub
        End Class
        Public Class MalContinue
            Inherits MalThrowable
        End Class

        ' Thrown by throw function
        Public Class MalException
            Inherits MalThrowable
            Private value As MalVal

            'string Message
            Public Sub New(new_value As MalVal)
                value = new_value
            End Sub
            Public Sub New(new_value As String)
                MyBase.New(new_value)
                value = New MalString(new_value)
            End Sub
            Public Function getValue() As MalVal
                return value
            End Function
        End Class

        '
        ' General functions
        '
        Public Shared Function _equal_Q(a As MalVal, b As MalVal) As Boolean
            Dim ota As Type = a.GetType()
            Dim otb As Type = b.GetType()
            If not (ota = otb Or
                (TypeOf a Is MalList and TypeOf b Is MalList)) Then
                return False
            Else
                If TypeOf a Is MalInt Then
                    return DirectCast(a,MalInt).getValue() =
                           DirectCast(b,MalInt).getValue()
                Else If TypeOf a Is MalSymbol Then
                    return DirectCast(a,MalSymbol).getName() =
                           DirectCast(b,MalSymbol).getName()
                Else If TypeOf a Is MalString Then
                    return DirectCast(a,MalString).getValue() =
                           DirectCast(b,MalString).getValue()
                Else If TypeOf a Is MalList Then
                    If DirectCast(a,MalList).size() <>
                       DirectCast(b,MalList).size()
                        return False
                    End If
                    for i As Integer = 0 To DirectCast(a,MalList).size()-1
                        If not _equal_Q(DirectCast(a,MalList)(i),
                                        DirectCast(b,MalList)(i))
                            return False
                        End If
                    Next
                    return True
                Else
                    return a Is b
                End If
            End If
        End Function


        Public MustInherit Class MalVal
            Private meta As MalVal = Nil
            Public Overridable Function copy() As MalVal
                return DirectCast(Me.MemberwiseClone(),MalVal)
            End Function

            ' Default is just to call regular toString()
            Public Overridable Function ToString() As String
                throw New MalException("ToString called on abstract MalVal")
            End Function
            Public Overridable Function ToString(print_readably As Boolean) As String
                return Me.ToString()
            End Function
            Public Function getMeta() As MalVal
                return meta
            End Function
            Public Function setMeta(m As MalVal) As MalVal
                meta = m
                return Me
            End Function
            Public Overridable Function list_Q() As Boolean
                return False
            End Function
        End Class

        Public Class MalConstant
            Inherits MalVal
            Private value As String
            Public Sub New(name As String)
                value = name
            End Sub
            Public Shadows Function copy() As MalConstant
                return Me
            End Function

            Public Overrides Function ToString() As String
                return value
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                return value
            End Function
        End Class

        Public Shared Nil As MalConstant = New MalConstant("nil")
        Public Shared MalTrue As MalConstant = New MalConstant("true")
        Public Shared MalFalse As MalConstant = New MalConstant("false")

        Public Class MalInt
            Inherits MalVal
            Private value As Int64
            Public Sub New(v As Int64)
                value = v
            End Sub
            Public Shadows Function copy() As MalInt 
                return Me
            End Function

            Public Function getValue() As Int64
                return value
            End Function
            Public Overrides Function ToString() As String
                return value.ToString()
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                return value.ToString()
            End Function
            Public Shared Operator <(a As MalInt, b As Malint) As MalConstant
                If a.getValue() < b.getValue() Then
                    return MalTrue
                Else
                    return MalFalse
                End If
            End Operator
            Public Shared Operator <=(a As MalInt, b As Malint) As MalConstant
                If a.getValue() <= b.getValue() Then
                    return MalTrue
                Else
                    return MalFalse
                End If
            End Operator
            Public Shared Operator >(a As MalInt, b As Malint) As MalConstant
                If a.getValue() > b.getValue() Then
                    return MalTrue
                Else
                    return MalFalse
                End If
            End Operator
            Public Shared Operator >=(a As MalInt, b As Malint) As MalConstant
                If a.getValue() >= b.getValue() Then
                    return MalTrue
                Else
                    return MalFalse
                End If
            End Operator
            Public Shared Operator +(a As MalInt, b As Malint) As MalInt
                return new MalInt(a.getValue() + b.getValue())
            End Operator
            Public Shared Operator -(a As MalInt, b As Malint) As MalInt
                return new MalInt(a.getValue() - b.getValue())
            End Operator
            Public Shared Operator *(a As MalInt, b As Malint) As MalInt
                return new MalInt(a.getValue() * b.getValue())
            End Operator
            Public Shared Operator /(a As MalInt, b As Malint) As MalInt
                return new MalInt(a.getValue() / b.getValue())
            End Operator
        End Class

        Public Class MalSymbol
            Inherits MalVal
            Private value As String
            Public Sub New(v As String)
                value = v
            End Sub
            Public Sub New(v As MalString)
                value = v.getValue()
            End Sub
            Public Shadows Function copy() As MalSymbol
                return Me
            End Function

            Public Function getName() As String
                return value
            End Function
            Public Overrides Function ToString() As String
                return value
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                return value
            End Function
        End Class

        Public Class MalString
            Inherits MalVal
            Private value As String
            Public Sub New(v As String)
                value = v
            End Sub
            Public Shadows Function copy() As MalString
                return Me
            End Function

            Public Function getValue() As String
                return value
            End Function
            Public Overrides Function ToString() As String
                return """" & value & """"
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                If value.Length > 0 AndAlso value(0) = ChrW(&H029e) Then
                    return ":" & value.Substring(1)
                Else If print_readably Then
                    return """" & _
                           value.Replace("\", "\\") _
                                .Replace("""", "\""") _
                                .Replace(Environment.NewLine, "\n") & _
                                """"
                Else
                    return value
                End If
            End Function
        End Class


        Public Class MalList
            Inherits MalVal
            Public start As String = "("
            Public last As String = ")"
            Private value As List(Of MalVal)
            Public Sub New()
                value = New List(Of MalVal)
            End Sub
            Public Sub New(val As List(Of MalVal))
                value = val
            End Sub
            Public Sub New(ParamArray mvs() As MalVal)
                value = New List(Of MalVal)
                conj_BANG(mvs)
            End Sub

            Public Function getValue() As List(Of MalVal)
                return value
            End Function
            Public Overrides Function list_Q() As Boolean
                return True
            End Function

            Public Overrides Function ToString() As String
                return start & printer.join(value, " ", true) & last
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                return start & printer.join(value, " ", print_readably) & last
            End Function

            Public Function conj_BANG(ParamArray mvs() As MalVal) As MalList 
                For i As Integer = 0 To mvs.Length-1
                    value.Add(mvs(i))
                Next
                return Me
            End Function

            Public Function size() As Int64
                return value.Count
            End Function
            Public Function nth(ByVal idx As Integer) As MalVal 
                If value.Count > idx Then
                    return value(idx)
                Else
                    return Nil
                End If
            End Function
            Default Public ReadOnly Property Item(idx As Integer) As MalVal
                Get
                    If value.Count > idx then
                        return value(idx)
                    Else
                        return Nil
                    End If
                End Get
            End Property
            Public Function rest() As MalList
                If size() > 0 Then
                    return New MalList(value.GetRange(1, value.Count-1))
                Else
                    return New MalList()
                End If
            End Function
            Public Overridable Function slice(start As Int64) As MalList 
                return New MalList(value.GetRange(start, value.Count-start))
            End Function
            Public Overridable Function slice(start As Int64, last As Int64) As MalList
                return New MalList(value.GetRange(start, last-start))
            End Function
        End Class

        Public Class MalVector
            Inherits MalList
'            ' Same implementation except for instantiation methods
            Public Sub New()
                MyBase.New()
                start = "["
                last = "]"
            End Sub
            Public Sub New(val As List(Of MalVal))
                MyBase.New(val)
                start = "["
                last = "]"
            End Sub

            Public Overrides Function list_Q() As Boolean
                return False
            End Function

            Public Overrides Function slice(start As Int64, last As Int64) As MalList
                Dim val As List(Of MalVal) = Me.getValue()
                return New MalVector(val.GetRange(start, val.Count-start))
            End Function
        End Class

        Public Class MalHashMap
            Inherits MalVal
            Private value As Dictionary(Of string, MalVal)
            Public Sub New(val As Dictionary(Of String, MalVal))
                value = val
            End Sub
            Public Sub New(lst As MalList)
                value = New Dictionary(Of String, MalVal)
                assoc_BANG(lst)
            End Sub
            Public Shadows Function copy() As MalHashMap 
                Dim new_self As MalHashMap = DirectCast(Me.MemberwiseClone(),MalHashMap)
                new_self.value = New Dictionary(Of String, MalVal)(value)
                return new_self
            End Function

            Public Function getValue() As Dictionary(Of String, MalVal)
                return value
            End Function

            Public Overrides Function ToString() As String
                return "{" & printer.join(value, " ", true) & "}"
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                return "{" & printer.join(value, " ", print_readably) & "}"
            End Function

            Public Function assoc_BANG(lst As MalList) As MalHashMap
                For i As Integer = 0 To lst.size()-1 Step 2
                    value(DirectCast(lst(i),MalString).getValue()) = lst(i+1)
                Next
                return Me
            End Function

            Public Function dissoc_BANG(lst As MalList) As MalHashMap
                for i As Integer = 0 To lst.size()-1
                    value.Remove(DirectCast(lst.nth(i),MalString).getValue())
                Next
                return Me
            End Function
        End Class

        Public Class MalAtom
            Inherits MalVal
            Private value As MalVal
            Public Sub New(val As MalVal)
                value = val
            End Sub
            'Public MalAtom copy() { return New MalAtom(value) }
            Public Function getValue() As MalVal
                return value
            End Function
            Public Function setValue(val As MalVal) As MalVal
                value = val
                return value
            End Function
            Public Overrides Function ToString() As String
                return "(atom " & printer._pr_str(value, true) & ")"
            End Function
            Public Overrides Function ToString(print_readably As Boolean) As String
                return "(atom " & printer._pr_str(value, print_readably) & ")"
            End Function
        End Class

        Public Class MalFunc
            Inherits MalVal
            Private fn As Func(Of MalList, MalVal) = Nothing
            Private ast As MalVal = Nothing
            Private env As Mal.env.Env = Nothing
            Private fparams As MalList
            Private macro As Boolean = False
            Public Sub New(new_fn As Func(Of MalList, MalVal))
                fn = new_fn
            End Sub
            Public Sub New(new_ast As MalVal, new_env As Mal.env.Env,
                           new_fparams As MalList, new_fn As Func(Of MalList, MalVal))
                fn = new_fn
                ast = new_ast
                env = new_env
                fparams = new_fparams
            End Sub

            Public Overrides Function ToString() As String
                If Not ast Is Nothing Then
                    return "<fn* " & Mal.printer._pr_str(fparams,true) &
                           " " & Mal.printer._pr_str(ast, true) & ">"
                Else
                    return "<builtin_function " & fn.ToString() & ">"
                End If
            End Function

            Public Function apply(args As MalList) As MalVal
                return fn(args)
            End Function

            Public Function getAst() As MalVal
                return ast
            End Function
            Public Function getEnv() As Mal.env.Env
                return env
            End Function
            Public Function getFParams() As MalList
                return fparams
            End Function
            Public Function genEnv(args As MalList) As Mal.env.Env
                return New Mal.env.Env(env, fparams, args)
            End Function
            Public Function isMacro() As Boolean
                return macro
            End Function
            Public Sub setMacro()
                macro = true
            End Sub
        End Class
    End Class
End Namespace
