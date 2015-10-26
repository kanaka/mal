package mal

import java.util.*

// TODO clean up exception hierarchy
open class MalException(message: String?) : Exception(message), MalType { }
class MalContinue() : MalException("continue") { }
class MalReaderException(message: String) : MalException(message) { }
class MalPrinterException(message: String) : MalException(message) { }

class MalCoreException(message: String, val value: MalType) : MalException(message) // TODO rename

interface MalType {
}

open class MalConstant(val value: String) : MalType {
    override fun equals(other: Any?): Boolean = other is MalConstant && value.equals(other.value)
}

class MalInteger(val value: Int) : MalType {
    operator fun plus(a: MalInteger): MalInteger = MalInteger(value + a.value)
    operator fun minus(a: MalInteger): MalInteger = MalInteger(value - a.value)
    operator fun times(a: MalInteger): MalInteger = MalInteger(value * a.value)
    operator fun div(a: MalInteger): MalInteger = MalInteger(value / a.value)
    operator fun compareTo(a: MalInteger): Int = value.compareTo(a.value)

    override fun equals(other: Any?): Boolean = other is MalInteger && value.equals(other.value)
}

class MalSymbol(val value: String) : MalType {
    override fun equals(other: Any?): Boolean = other is MalSymbol && value.equals(other.value)
}

open class MalString(value: String) : MalConstant(value)

class MalKeyword(value: String) : MalString("\u029E" + value)

interface ILambda : MalType {
    fun apply(seq: ISeq): MalType
}

open class MalFunction(val lambda: (ISeq) -> MalType) : MalType, ILambda {
    var is_macro: Boolean = false

    override fun apply(seq: ISeq): MalType = lambda(seq)
}

class MalFnFunction(val ast: MalType, val params: Sequence<MalSymbol>, val env: Env, lambda: (ISeq) -> MalType) : MalFunction(lambda)

interface ISeq : MalType {
    fun seq(): Sequence<MalType>
    fun first(): MalType
    fun rest(): ISeq
    fun nth(n: Int): MalType
    fun slice(fromIndex: Int, toIndex: Int): ISeq
}

interface IMutableSeq : ISeq {
    fun conj_BANG(form: MalType)
}

class MalSequence(val elements : Sequence<MalType>) : MalType, ISeq {
    override fun seq(): Sequence<MalType> = elements
    override fun first(): MalType = elements.first()
    override fun rest(): ISeq = MalSequence(elements.drop(1))
    override fun nth(n: Int): MalType = elements.elementAt(n)

    override fun slice(fromIndex: Int, toIndex: Int): MalList =
            MalList(elements.toLinkedList().subList(fromIndex, toIndex))
}

class MalList(val elements: MutableList<MalType>) : MalType, IMutableSeq {
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
}

class MalVector(val elements: MutableList<MalType>) : MalType, IMutableSeq {
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
}

class MalHashMap : MalType {
    val elements = HashMap<MalString, MalType>()

    fun assoc_BANG(key: MalString, value: MalType) = elements.put(key, value)
}

// TODO add truthiness checking
val NIL = MalConstant("nil")
val TRUE = MalConstant("true")
val FALSE = MalConstant("false")
