package mal

import java.util.*

open class MalException(message: String) : Exception(message) { }
class MalContinue() : MalException("continue") { }
class MalReaderException(message: String) : MalException(message) { }
class MalPrinterException(message: String) : MalException(message) { }

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

class MalSymbol(value: String) : MalConstant(value)

open class MalString(value: String) : MalConstant(value)

class MalKeyword(value: String) : MalString("\u029E" + value)

interface ILambda : MalType {
    fun apply(seq: ISeq): MalType
}

class MalFunction(val lambda: (ISeq) -> MalType) : MalType, ILambda {
    override fun apply(seq: ISeq): MalType = lambda(seq)
}

interface ISeq : MalType {
    fun seq(): Sequence<MalType>
    fun first(): MalType
    fun rest(): ISeq
    fun nth(n: Int): MalType
}

interface IMutableSeq : ISeq {
    fun conj_BANG(form: MalType)
}

class MalSequence(val elements : Sequence<MalType>) : MalType, ISeq {
    override fun seq(): Sequence<MalType> = elements
    override fun first(): MalType = elements.first()
    override fun rest(): ISeq = MalSequence(elements.drop(1))
    override fun nth(n: Int): MalType = elements.elementAt(n)
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
}

class MalHashMap : MalType {
    val elements = HashMap<MalString, MalType>()

    fun assoc_BANG(key: MalString, value: MalType) = elements.put(key, value)
}

// TODO add truthiness checking
val NIL = MalConstant("nil")
val TRUE = MalConstant("true")
val FALSE = MalConstant("false")
