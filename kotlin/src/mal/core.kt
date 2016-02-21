package mal

import java.io.File
import java.util.*

val ns = hashMapOf(
        envPair("+", { a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger + y as MalInteger }) }),
        envPair("-", { a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger - y as MalInteger }) }),
        envPair("*", { a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger * y as MalInteger }) }),
        envPair("/", { a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger / y as MalInteger }) }),

        envPair("list", { a: ISeq -> MalList(a) }),
        envPair("list?", { a: ISeq -> if (a.first() is MalList) TRUE else FALSE }),
        envPair("empty?", { a: ISeq -> if (a.first() !is ISeq || !(a.first() as ISeq).seq().any()) TRUE else FALSE }),
        envPair("count", { a: ISeq ->
            if (a.first() is ISeq) MalInteger((a.first() as ISeq).count().toLong()) else MalInteger(0)
        }),

        envPair("=", { a: ISeq -> pairwiseEquals(a) }),
        envPair("<", { a: ISeq -> pairwiseCompare(a, { x, y -> x.value < y.value }) }),
        envPair("<=", { a: ISeq -> pairwiseCompare(a, { x, y -> x.value <= y.value }) }),
        envPair(">", { a: ISeq -> pairwiseCompare(a, { x, y -> x.value > y.value }) }),
        envPair(">=", { a: ISeq -> pairwiseCompare(a, { x, y -> x.value >= y.value }) }),

        envPair("pr-str", { a: ISeq ->
            MalString(a.seq().map({ it -> pr_str(it, print_readably = true) }).joinToString(" "))
        }),
        envPair("str", { a: ISeq ->
            MalString(a.seq().map({ it -> pr_str(it, print_readably = false) }).joinToString(""))
        }),
        envPair("prn", { a: ISeq ->
            println(a.seq().map({ it -> pr_str(it, print_readably = true) }).joinToString(" "))
            NIL
        }),
        envPair("println", { a: ISeq ->
            println(a.seq().map({ it -> pr_str(it, print_readably = false) }).joinToString(" "))
            NIL
        }),

        envPair("read-string", { a: ISeq ->
            val string = a.first() as? MalString ?: throw MalException("slurp requires a string parameter")
            read_str(string.value)
        }),
        envPair("slurp", { a: ISeq ->
            val name = a.first() as? MalString ?: throw MalException("slurp requires a filename parameter")
            val text = File(name.value).readText()
            MalString(text)
        }),

        envPair("cons", { a: ISeq ->
            val list = a.nth(1) as? ISeq ?: throw MalException("cons requires a list as its second parameter")
            val mutableList = list.seq().toCollection(LinkedList<MalType>())
            mutableList.addFirst(a.nth(0))
            MalList(mutableList)
        }),
        envPair("concat", { a: ISeq -> MalList(a.seq().flatMap({ it -> (it as ISeq).seq() }).toCollection(LinkedList<MalType>())) }),

        envPair("nth", { a: ISeq ->
            val list = a.nth(0) as? ISeq ?: throw MalException("nth requires a list as its first parameter")
            val index = a.nth(1) as? MalInteger ?: throw MalException("nth requires an integer as its second parameter")
            if (index.value >= list.count()) throw MalException("index out of bounds")
            list.nth(index.value.toInt())
        }),
        envPair("first", { a: ISeq ->
            if (a.nth(0) == NIL) NIL
            else {
                val list = a.nth(0) as? ISeq ?: throw MalException("first requires a list parameter")
                if (list.seq().any()) list.first() else NIL
            }
        }),
        envPair("rest", { a: ISeq ->
            if (a.nth(0) == NIL) MalList()
            else {
                val list = a.nth(0) as? ISeq ?: throw MalException("rest requires a list parameter")
                MalList(list.rest())
            }
        }),

        envPair("throw", { a: ISeq ->
            val throwable = a.nth(0)
            throw MalCoreException(pr_str(throwable), throwable)
        }),

        envPair("apply", { a: ISeq ->
            val function = a.nth(0) as MalFunction
            val params = MalList()
            a.seq().drop(1).forEach({ it ->
                if (it is ISeq) {
                    it.seq().forEach({ x -> params.conj_BANG(x) })
                } else {
                    params.conj_BANG(it)
                }
            })
            function.apply(params)
        }),

        envPair("map", { a: ISeq ->
            val function = a.nth(0) as MalFunction
            MalList((a.nth(1) as ISeq).seq().map({ it ->
                val params = MalList()
                params.conj_BANG(it)
                function.apply(params)
            }).toCollection(LinkedList<MalType>()))
        }),

        envPair("nil?", { a: ISeq -> if (a.nth(0) == NIL) TRUE else FALSE }),
        envPair("true?", { a: ISeq -> if (a.nth(0) == TRUE) TRUE else FALSE }),
        envPair("false?", { a: ISeq -> if (a.nth(0) == FALSE) TRUE else FALSE }),
        envPair("symbol?", { a: ISeq -> if (a.nth(0) is MalSymbol) TRUE else FALSE }),

        envPair("symbol", { a: ISeq -> MalSymbol((a.nth(0) as MalString).value) }),
        envPair("keyword", { a: ISeq ->
            val param = a.nth(0)
            if (param is MalKeyword) param else MalKeyword((a.nth(0) as MalString).value)
        }),
        envPair("keyword?", { a: ISeq -> if (a.nth(0) is MalKeyword) TRUE else FALSE }),
        envPair("vector", { a: ISeq -> MalVector(a) }),
        envPair("vector?", { a: ISeq -> if (a.nth(0) is MalVector) TRUE else FALSE }),

        envPair("hash-map", { a: ISeq ->
            val map = MalHashMap()
            pairwise(a).forEach({ it -> map.assoc_BANG(it.first as MalString, it.second) })
            map
        }),
        envPair("map?", { a: ISeq -> if (a.nth(0) is MalHashMap) TRUE else FALSE }),
        envPair("assoc", { a: ISeq ->
            val map = MalHashMap(a.first() as MalHashMap)
            pairwise(a.rest()).forEach({ it -> map.assoc_BANG(it.first as MalString, it.second) })
            map
        }),
        envPair("dissoc", { a: ISeq ->
            val map = MalHashMap(a.first() as MalHashMap)
            a.rest().seq().forEach({ it -> map.dissoc_BANG(it as MalString) })
            map
        }),
        envPair("get", { a: ISeq ->
            val map = a.nth(0) as? MalHashMap
            val key = a.nth(1) as MalString
            map?.elements?.get(key) ?: NIL
        }),
        envPair("contains?", { a: ISeq ->
            val map = a.nth(0) as? MalHashMap
            val key = a.nth(1) as MalString
            if (map?.elements?.get(key) != null) TRUE else FALSE
        }),
        envPair("keys", { a: ISeq ->
            val map = a.nth(0) as MalHashMap
            MalList(map.elements.keys.toCollection(LinkedList<MalType>()))
        }),
        envPair("vals", { a: ISeq ->
            val map = a.nth(0) as MalHashMap
            MalList(map.elements.values.toCollection(LinkedList<MalType>()))
        }),
        envPair("count", { a: ISeq ->
            val seq = a.nth(0) as? ISeq
            if (seq != null) MalInteger(seq.count().toLong()) else ZERO
        }),
        envPair("sequential?", { a: ISeq -> if (a.nth(0) is ISeq) TRUE else FALSE }),

        envPair("with-meta", { a: ISeq ->
            val obj = a.nth(0)
            val metadata = a.nth(1)
            obj.with_meta(metadata)
        }),
        envPair("meta", { a: ISeq -> a.first().metadata }),
        envPair("conj", { a: ISeq -> (a.first() as ISeq).conj(a.rest()) }),

        envPair("atom", { a: ISeq -> MalAtom(a.first()) }),
        envPair("atom?", { a: ISeq -> if (a.first() is MalAtom) TRUE else FALSE }),
        envPair("deref", { a: ISeq -> (a.first() as MalAtom).value }),
        envPair("reset!", { a: ISeq ->
            val atom = a.nth(0) as MalAtom
            val value = a.nth(1)
            atom.value = value
            value
        }),
        envPair("swap!", { a: ISeq ->
            val atom = a.nth(0) as MalAtom
            val function = a.nth(1) as MalFunction

            val params = MalList()
            params.conj_BANG(atom.value)
            a.seq().drop(2).forEach({ it -> params.conj_BANG(it) })

            val value = function.apply(params)
            atom.value = value

            value
        }),

        envPair("readline", { a: ISeq ->
            val prompt = a.first() as MalString
            try {
                MalString(readline(prompt.value))
            } catch (e: java.io.IOException) {
                throw MalException(e.message)
            } catch (e: EofException) {
                NIL
            }
        }),

        envPair("time-ms", { a: ISeq -> MalInteger(System.currentTimeMillis()) })
)

private fun envPair(k: String, v: (ISeq) -> MalType): Pair<MalSymbol, MalType> = Pair(MalSymbol(k), MalFunction(v))

private fun pairwise(s: ISeq): List<Pair<MalType, MalType>> {
    val (keys, vals) = s.seq().withIndex().partition({ it -> it.index % 2 == 0 })
    return keys.map({ it -> it.value }).zip(vals.map({ it -> it.value }))
}

private fun pairwiseCompare(s: ISeq, pred: (MalInteger, MalInteger) -> Boolean): MalConstant =
        if (pairwise(s).all({ it -> pred(it.first as MalInteger, it.second as MalInteger) })) TRUE else FALSE

private fun pairwiseEquals(s: ISeq): MalConstant =
        if (pairwise(s).all({ it -> it.first == it.second })) TRUE else FALSE
