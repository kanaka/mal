Imports System
Imports System.IO
Imports System.Collections.Generic
Imports MalVal = Mal.types.MalVal
Imports MalConstant = Mal.types.MalConstant
Imports MalInt = Mal.types.MalInt
Imports MalSymbol = Mal.types.MalSymbol
Imports MalString = Mal.types.MalString
Imports MalList = Mal.types.MalList
Imports MalVector = Mal.types.MalVector
Imports MalHashMap = Mal.types.MalHashMap
Imports MalAtom = Mal.types.MalAtom
Imports MalFunc = Mal.types.MalFunc

Namespace Mal
    Public Class core
        Shared Nil As MalConstant = Mal.types.Nil
        Shared MalTrue As MalConstant = Mal.types.MalTrue
        Shared MalFalse As MalConstant = Mal.types.MalFalse

        ' Errors/Exceptions
        Shared Function mal_throw(a As MalList) As MalVal
            throw New Mal.types.MalException(a(0))
        End Function

        ' General functions
        Shared Function equal_Q(a As MalList) As MalVal
            If Mal.types._equal_Q(a(0), a(1)) Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        ' Scalar functions
        Shared Function nil_Q(a As MalList) As MalVal
            If a(0) Is Nil Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function true_Q(a As MalList) As MalVal
            If a(0) Is MalTrue Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function false_Q(a As MalList) As MalVal
            If a(0) Is MalFalse Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function symbol(a As MalList) As MalVal
            return new MalSymbol(DirectCast(a(0),MalString))
        End Function

        Shared Function symbol_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalSymbol Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function keyword(a As MalList) As MalVal
            Dim s As String = DirectCast(a(0),MalString).getValue()
            return new MalString(ChrW(&H029e) & s)
        End Function

        Shared Function keyword_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalString Then
                Dim s As String = DirectCast(a(0),MalString).getValue()
                If s.Substring(0,1) = Strings.ChrW(&H029e) Then
                    return MalTrue
                Else
                    return MalFalse
                End If
            Else
                return MalFalse
            End If
        End Function


        ' Number functions
        Shared Function lt(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) <  DirectCast(a(1),MalInt)
        End Function
        Shared Function lte(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) <= DirectCast(a(1),MalInt)
        End Function
        Shared Function gt(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) >  DirectCast(a(1),MalInt)
        End Function
        Shared Function gte(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) >= DirectCast(a(1),MalInt)
        End Function
        Shared Function plus(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) +  DirectCast(a(1),MalInt)
        End Function
        Shared Function minus(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) -  DirectCast(a(1),MalInt)
        End Function
        Shared Function mult(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) *  DirectCast(a(1),MalInt)
        End Function
        Shared Function div(a As MalList) As MalVal
            return DirectCast(a(0),MalInt) /  DirectCast(a(1),MalInt)
        End Function

        Shared Function time_ms(a As MalList) As MalVal
            return New MalInt(DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond)
        End Function

        ' String functions
        Shared Function pr_str(a As MalList) As MalVal
            return New MalString(printer._pr_str_args(a, " ", true))
        End Function

        Shared Function str(a As MalList) As MalVal
            return new MalString(printer._pr_str_args(a, "", false))
        End Function

        Shared Function prn(a As MalList) As MalVal
            Console.WriteLine(printer._pr_str_args(a, " ", true))
            return Nil
        End Function

        Shared Function println(a As MalList) As MalVal
            Console.WriteLine(printer._pr_str_args(a, " ", false))
            return Nil
        End Function

        Shared Function mal_readline(a As MalList) As MalVal
            Dim line As String
            line = readline.Readline(DirectCast(a(0),MalString).getValue())
            If line Is Nothing Then
                return types.Nil
            Else
                return New MalString(line)
            End If
        End Function

        Shared Function read_string(a As MalList) As MalVal
            return reader.read_str(DirectCast(a(0),MalString).getValue())
        End Function

        Shared Function slurp(a As MalList) As MalVal
            return New MalString(File.ReadAllText(DirectCast(a(0),MalString).getValue()))
        End Function


        ' List/Vector functions

        Shared Function list(a As MalList) As MalVal
            return New MalList(a.getValue())
        End Function

        Shared Function list_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalList And Not TypeOf a(0) Is MalVector Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function vector(a As MalList) As MalVal
            return New MalVector(a.getValue())
        End Function

        Shared Function vector_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalVector Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        ' HashMap functions
        Shared Function hash_map(a As MalList) As MalVal
            return New MalHashMap(a)
        End Function

        Shared Function hash_map_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalHashMap Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function contains_Q(a As MalList) As MalVal
            Dim key As String = DirectCast(a(1),MalString).getValue()
            Dim dict As Dictionary(Of String,MalVal) = DirectCast(a(0),MalHashMap).getValue()
            If dict.ContainsKey(key) Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function assoc(a As MalList) As MalVal
            Dim new_hm As MalHashMap = DirectCast(a(0),MalHashMap).copy()
            return new_hm.assoc_BANG(DirectCast(a.slice(1),MalList))
        End Function

        Shared Function dissoc(a As MalList) As MalVal
            Dim new_hm As MalHashMap = DirectCast(a(0),MalHashMap).copy()
            return new_hm.dissoc_BANG(DirectCast(a.slice(1),MalList))
        End Function

        Shared Function do_get(a As MalList) As MalVal
            Dim k As String = DirectCast(a(1),MalString).getValue()
            If a(0) Is Nil Then
                return Nil
            Else
                Dim dict As Dictionary(Of String,MalVal) = DirectCast(a(0),MalHashMap).getValue()
                If dict.ContainsKey(k) Then
                    return dict(k)
                Else
                    return Nil
                End If
            End If
        End Function

        Shared Function keys(a As MalList) As MalVal
            Dim dict As Dictionary(Of String,MalVal) = DirectCast(a(0),MalHashMap).getValue()
            Dim key_lst As MalList = New MalList()
            For Each key As String in dict.Keys
                key_lst.conj_BANG(new MalString(key))
            Next
            return key_lst
        End Function

        Shared Function vals(a As MalList) As MalVal
            Dim dict As Dictionary(Of String,MalVal) = DirectCast(a(0),MalHashMap).getValue()
            Dim val_lst As MalList = New MalList()
            For Each val As MalVal In dict.Values
                val_lst.conj_BANG(val)
            Next
            return val_lst
        End Function

        ' Sequence functions
        Shared Function sequential_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalList Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function cons(a As MalList) As MalVal
            Dim lst As New List(Of MalVal)
            lst.Add(a(0))
            lst.AddRange(DirectCast(a(1),MalList).getValue())
            return DirectCast(New MalList(lst),MalVal)
        End Function

        Shared Function concat(a As MalList) As MalVal
            If a.size() = 0 Then
                return new MalList()
            End If
            Dim lst As New List(Of MalVal)
            lst.AddRange(DirectCast(a(0),MalList).getValue())
            for i As Integer = 1 To a.size()-1
                lst.AddRange(DirectCast(a(i),MalList).getValue())
            Next
            return DirectCast(new MalList(lst),MalVal)
        End Function

        Shared Function nth(a As MalList) As MalVal
            Dim idx As Integer = DirectCast(a(1),MalInt).getValue()
            If (idx < DirectCast(a(0),MalList).size()) Then
                return DirectCast(a(0),MalList)( idx )
            Else
                throw new Mal.types.MalException(
                    "nth: index out of range")
            End If
        End Function

        Shared Function first(a As MalList) As MalVal
            return DirectCast(a(0),MalList)(0)
        End Function

        Shared Function rest(a As MalList) As MalVal
            return DirectCast(a(0),MalList).rest()
        End Function

        Shared Function empty_Q(a As MalList) As MalVal
            If DirectCast(a(0),MalList).size() = 0 Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function count(a As MalList) As MalVal
            If a(0) Is Nil Then
                return new MalInt(0)
            Else
                return new MalInt(DirectCast(a(0),MalList).size())
            End If
        End Function

        Shared Function conj(a As MalList) As MalVal
            Dim src_lst As List(Of MalVal) = DirectCast(a(0),MalList).getValue()
            Dim new_lst As New List(Of MalVal)
            new_lst.AddRange(src_lst)
            If TypeOf a(0) Is MalVector Then
                For i As Integer = 1 To a.size()-1
                    new_lst.Add(a(i))
                Next
                return new MalVector(new_lst)
            Else
                For i As Integer = 1 To a.size()-1
                    new_lst.Insert(0, a(i))
                Next
                return new MalList(new_lst)
            End If
        End Function


        ' General list related functions
        Shared Function apply(a As MalList) As MalVal
            Dim f As MalFunc = DirectCast(a(0),MalFunc)
            Dim lst As New List(Of MalVal)
            lst.AddRange(a.slice(1,a.size()-1).getValue())
            lst.AddRange(DirectCast(a(a.size()-1),MalList).getValue())
            return f.apply(New MalList(lst))
        End Function

        Shared Function map(a As MalList) As MalVal
            Dim f As MalFunc = DirectCast(a(0),MalFunc)
            Dim src_lst As List(Of MalVal) = DirectCast(a(1),MalList).getValue()
            Dim new_lst As New List(Of MalVal)
            for i As Integer = 0 To  src_lst.Count-1
                new_lst.Add(f.apply(New MalList(src_lst(i))))
            Next
            return new MalList(new_lst)
        End Function


        ' Metadata functions
        Shared Function atom(a As MalList) As MalVal
            return new MalAtom(a(0))
        End Function

        Shared Function meta(a As MalList) As MalVal
            return a(0).getMeta()
        End Function

        Shared Function with_meta(a As MalList) As MalVal
            return DirectCast(a(0),MalVal).copy().setMeta(a(1))
        End Function


        ' Atom functions
        Shared Function atom_Q(a As MalList) As MalVal
            If TypeOf a(0) Is MalAtom Then
                return MalTrue
            Else
                return MalFalse
            End If
        End Function

        Shared Function deref(a As MalList) As MalVal
            return DirectCast(a(0),MalAtom).getValue()
        End Function

        Shared Function reset_BANG(a As MalList) As MalVal
            return DirectCast(a(0),MalAtom).setValue(a(1))
        End Function

        Shared Function swap_BANG(a As MalList) As MalVal
            Dim atm As MalAtom = DirectCast(a(0),MalAtom)
            Dim f As MalFunc = DirectCast(a(1),MalFunc)
            Dim new_lst As New List(Of MalVal)
            new_lst.Add(atm.getValue())
            new_lst.AddRange(DirectCast(a.slice(2),MalList).getValue())
            return atm.setValue(f.apply(New MalList(new_lst)))
        End Function



        Shared Function ns As Dictionary(Of String, MalVal)
            Dim ns As New Dictionary(Of String, MalVal)

            ns.Add("=", New MalFunc(AddressOf equal_Q))
            ns.Add("throw", New MalFunc(AddressOf mal_throw))
            ns.Add("nil?", New MalFunc(AddressOf nil_Q))
            ns.Add("true?", New MalFunc(AddressOf true_Q))
            ns.Add("false?", New MalFunc(AddressOf false_Q))
            ns.Add("symbol", new MalFunc(AddressOf symbol))
            ns.Add("symbol?", New MalFunc(AddressOf symbol_Q))
            ns.Add("keyword", new MalFunc(AddressOf keyword))
            ns.Add("keyword?", New MalFunc(AddressOf keyword_Q))

            ns.Add("pr-str",New MalFunc(AddressOf pr_str))
            ns.Add("str", New MalFunc(AddressOf str))
            ns.Add("prn", New MalFunc(AddressOf prn))
            ns.Add("println", New MalFunc(AddressOf println))
            ns.Add("readline", New MalFunc(AddressOf mal_readline))
            ns.Add("read-string", New MalFunc(AddressOf read_string))
            ns.Add("slurp", New MalFunc(AddressOf slurp))
            ns.Add("<",  New MalFunc(AddressOf lt))
            ns.Add("<=", New MalFunc(AddressOf lte))
            ns.Add(">",  New MalFunc(AddressOf gt))
            ns.Add(">=", New MalFunc(AddressOf gte))
            ns.Add("+",  New MalFunc(AddressOf plus))
            ns.Add("-",  New MalFunc(AddressOf minus))
            ns.Add("*",  New MalFunc(AddressOf mult))
            ns.Add("/",  New MalFunc(AddressOf div))
            ns.Add("time-ms", New MalFunc(AddressOf time_ms))

            ns.Add("list",  New MalFunc(AddressOf list))
            ns.Add("list?", New MalFunc(AddressOf list_Q))
            ns.Add("vector", new MalFunc(AddressOf vector))
            ns.Add("vector?", New MalFunc(AddressOf vector_Q))
            ns.Add("hash-map", new MalFunc(AddressOf hash_map))
            ns.Add("map?", New MalFunc(AddressOf hash_map_Q))
            ns.Add("contains?", New MalFunc(AddressOf contains_Q))
            ns.Add("assoc", New MalFunc(AddressOf assoc))
            ns.Add("dissoc", New MalFunc(AddressOf dissoc))
            ns.Add("get", New MalFunc(AddressOf do_get))
            ns.Add("keys", New MalFunc(AddressOf keys))
            ns.Add("vals", New MalFunc(AddressOf vals))

            ns.Add("sequential?", New MalFunc(AddressOf sequential_Q))
            ns.Add("cons", New MalFunc(AddressOf cons))
            ns.Add("concat", New MalFunc(AddressOf concat))
            ns.Add("nth", New MalFunc(AddressOf nth))
            ns.Add("first", New MalFunc(AddressOf first))
            ns.Add("rest",  New MalFunc(AddressOf rest))
            ns.Add("empty?", New MalFunc(AddressOf empty_Q))
            ns.Add("count",New MalFunc(AddressOf  count))
            ns.Add("conj", New MalFunc(AddressOf conj))
            ns.Add("apply", New MalFunc(AddressOf apply))
            ns.Add("map", New MalFunc(AddressOf map))

            ns.Add("with-meta", New MalFunc(AddressOf with_meta))
            ns.Add("meta", New MalFunc(AddressOf meta))
            ns.Add("atom", new MalFunc(AddressOf atom))
            ns.Add("atom?", New MalFunc(AddressOf atom_Q))
            ns.Add("deref", New MalFunc(AddressOf deref))
            ns.Add("reset!", New MalFunc(AddressOf reset_BANG))
            ns.Add("swap!", New MalFunc(AddressOf swap_BANG))
            return ns
        End Function
    End Class
End Namespace
