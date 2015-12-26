package mal

import java.util.*

class Env(val outer: Env?, binds: Sequence<MalSymbol>?, exprs: Sequence<MalType>?) {
    val data = HashMap<String, MalType>()

    init {
        if (binds != null && exprs != null) {
            val itb = binds.iterator()
            val ite = exprs.iterator()
            while (itb.hasNext()) {
                val b = itb.next()
                if (b.value != "&") {
                    set(b, if (ite.hasNext()) ite.next() else NIL)
                } else {
                    if (!itb.hasNext()) throw MalException("expected a symbol name for varargs")
                    set(itb.next(), MalList(ite.asSequence().toCollection(LinkedList<MalType>())))
                    break
                }
            }
        }
    }

    constructor() : this(null, null, null)
    constructor(outer: Env?) : this(outer, null, null)

    fun set(key: MalSymbol, value: MalType): MalType {
        data.put(key.value, value)
        return value
    }

    fun find(key: MalSymbol): MalType? = data[key.value] ?: outer?.find(key)

    fun get(key: MalSymbol): MalType = find(key) ?: throw MalException("'${key.value}' not found")
}
