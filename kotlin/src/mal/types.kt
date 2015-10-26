package mal

import java.util.*

// TODO clean up exception hierarchy
open class MalException(message: String?) : Exception(message), MalType {
    override var metadata: MalType = NIL
    override fun with_meta(meta: MalType): MalType {
        val exception = MalException(message)
        exception.metadata = meta
        return exception
    }
}
class MalContinue() : MalException("continue") { }
class MalReaderException(message: String) : MalException(message) { }
class MalPrinterException(message: String) : MalException(message) { }
class MalCoreException(message: String, val value: MalType) : MalException(message) { // TODO rename
    override fun with_meta(meta: MalType): MalType {
        val exception = MalCoreException(message as String, value)
        exception.metadata = meta
        return exception
    }
}

interface MalType {
    var metadata: MalType // TODO make immutable
    fun with_meta(meta: MalType): MalType
}

open class MalConstant(val value: String) : MalType {
    override var metadata: MalType = NIL

    override fun equals(other: Any?): Boolean = other is MalConstant && value.equals(other.value)
    override fun hashCode(): Int = value.hashCode()

    override fun with_meta(meta: MalType): MalType {
        val obj = MalConstant(value)
        obj.metadata = meta
        return obj
    }
}

class MalInteger(val value: Int) : MalType {
    override var metadata: MalType = NIL

    operator fun plus(a: MalInteger): MalInteger = MalInteger(value + a.value)
    operator fun minus(a: MalInteger): MalInteger = MalInteger(value - a.value)
    operator fun times(a: MalInteger): MalInteger = MalInteger(value * a.value)
    operator fun div(a: MalInteger): MalInteger = MalInteger(value / a.value)
    operator fun compareTo(a: MalInteger): Int = value.compareTo(a.value)

    override fun equals(other: Any?): Boolean = other is MalInteger && value.equals(other.value)

    override fun with_meta(meta: MalType): MalType {
        val obj = MalInteger(value)
        obj.metadata = meta
        return obj
    }
}

class MalSymbol(val value: String) : MalType {
    override var metadata: MalType = NIL

    override fun equals(other: Any?): Boolean = other is MalSymbol && value.equals(other.value)

    override fun with_meta(meta: MalType): MalType {
        val obj = MalSymbol(value)
        obj.metadata = meta
        return obj
    }
}

open class MalString(value: String) : MalConstant(value) {
    override fun with_meta(meta: MalType): MalType {
        val obj = MalString(value)
        obj.metadata = meta
        return obj
    }
}

class MalKeyword(value: String) : MalString("\u029E" + value) {
    override fun with_meta(meta: MalType): MalType {
        val obj = MalKeyword(value)
        obj.metadata = meta
        return obj
    }
}

interface ILambda : MalType {
    fun apply(seq: ISeq): MalType
}

open class MalFunction(val lambda: (ISeq) -> MalType) : MalType, ILambda {
    // TODO make this stuff immutable?
    var is_macro: Boolean = false
    override var metadata: MalType = NIL

    override fun apply(seq: ISeq): MalType = lambda(seq)

    override fun with_meta(meta: MalType): MalType {
        val obj = MalFunction(lambda)
        obj.metadata = meta
        return obj
    }
}

class MalFnFunction(val ast: MalType, val params: Sequence<MalSymbol>, val env: Env, lambda: (ISeq) -> MalType) : MalFunction(lambda) {
    override fun with_meta(meta: MalType): MalType {
        val obj = MalFnFunction(ast, params, env, lambda)
        obj.metadata = meta
        return obj
    }
}

interface ISeq : MalType {
    fun seq(): Sequence<MalType>
    fun first(): MalType
    fun rest(): ISeq
    fun nth(n: Int): MalType
    fun slice(fromIndex: Int, toIndex: Int): ISeq
    fun conj(s: ISeq): ISeq
}

// TODO could we get rid of this and make conj work on immutables?
interface IMutableSeq : ISeq {
    fun conj_BANG(form: MalType)
}

class MalSequence(val elements : Sequence<MalType>) : MalType, ISeq {
    override var metadata: MalType = NIL

    override fun seq(): Sequence<MalType> = elements
    override fun first(): MalType = elements.first()
    override fun rest(): ISeq = MalSequence(elements.drop(1))
    override fun nth(n: Int): MalType = elements.elementAt(n)

    override fun slice(fromIndex: Int, toIndex: Int): MalList =
            MalList(elements.toLinkedList().subList(fromIndex, toIndex))

    override fun conj(s: ISeq): ISeq = MalList(elements.toLinkedList()).conj(s)

    override fun with_meta(meta: MalType): MalType {
        val obj = MalSequence(elements)
        obj.metadata = meta
        return obj
    }
}

class MalList(val elements: MutableList<MalType>) : MalType, IMutableSeq {
    override var metadata: MalType = NIL

    constructor() : this(LinkedList<MalType>())
    constructor(s: ISeq) : this(s.seq().toLinkedList())

    override fun seq(): Sequence<MalType> = elements.asSequence()
    override fun first(): MalType = elements.first()
    override fun rest(): ISeq = MalSequence(elements.drop(1).asSequence())
    override fun nth(n: Int): MalType = elements.elementAt(n)

    override fun conj_BANG(form: MalType) {
        elements.add(form)
    }

    override fun equals(other: Any?): Boolean =
            (other is ISeq)
                    && elements.size == other.seq().count() // TODO optimize counting?
                    && elements.asSequence().zip(other.seq()).all({ it -> it.first == it.second })

    override fun slice(fromIndex: Int, toIndex: Int): MalList =
            MalList(elements.subList(fromIndex, toIndex))

    override fun conj(s: ISeq): ISeq {
        val list = LinkedList<MalType>(elements)
        s.seq().forEach({ it -> list.addFirst(it) })
        return MalList(list)
    }

    override fun with_meta(meta: MalType): MalType {
        val obj = MalList(elements)
        obj.metadata = meta
        return obj
    }
}

class MalVector(val elements: MutableList<MalType>) : MalType, IMutableSeq {
    override var metadata: MalType = NIL

    constructor() : this(ArrayList<MalType>())
    constructor(s: ISeq) : this(s.seq().toArrayList())

    override fun seq(): Sequence<MalType> = elements.asSequence()
    override fun first(): MalType = elements.first()
    override fun rest(): ISeq = MalSequence(elements.drop(1).asSequence())
    override fun nth(n: Int): MalType = elements.elementAt(n)

    override fun conj_BANG(form: MalType) {
        elements.add(form)
    }

    override fun equals(other: Any?): Boolean =
            (other is ISeq)
                    && elements.size == other.seq().count() // TODO optimize counting?
                    && elements.asSequence().zip(other.seq()).all({ it -> it.first == it.second })

    override fun slice(fromIndex: Int, toIndex: Int): MalVector =
            MalVector(elements.subList(fromIndex, toIndex))

    override fun conj(s: ISeq): ISeq = MalVector(elements.plus(s.seq()).toArrayList())

    override fun with_meta(meta: MalType): MalType {
        val obj = MalVector(elements)
        obj.metadata = meta
        return obj
    }
}

class MalHashMap() : MalType {
    override var metadata: MalType = NIL

    val elements = HashMap<MalString, MalType>()

    constructor(other: MalHashMap) : this() {
        other.elements.forEach({ it -> assoc_BANG(it.key, it.value) })
    }

    fun assoc_BANG(key: MalString, value: MalType) = elements.put(key, value)
    fun dissoc_BANG(key: MalString) {
        elements.remove(key)
    }

    override fun with_meta(meta: MalType): MalType {
        val obj = MalHashMap(this)
        obj.metadata = meta
        return obj
    }
}

// TODO add truthiness checking
val NIL = MalConstant("nil")
val TRUE = MalConstant("true")
val FALSE = MalConstant("false")
val ZERO = MalInteger(0)
