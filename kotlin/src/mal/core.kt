package mal

import java.io.File

val ns = hashMapOf(
        Pair(MalSymbol("+"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger + y as MalInteger }) })),
        Pair(MalSymbol("-"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger - y as MalInteger }) })),
        Pair(MalSymbol("*"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger * y as MalInteger }) })),
        Pair(MalSymbol("/"), MalFunction({ a: ISeq -> a.seq().reduce({ x, y -> x as MalInteger / y as MalInteger }) })),

        Pair(MalSymbol("list"), MalFunction({ a: ISeq -> MalList(a) })),
        Pair(MalSymbol("list?"), MalFunction({ a: ISeq -> if (a.first() is MalList) TRUE else FALSE })),
        Pair(MalSymbol("empty?"), MalFunction({
            a: ISeq -> if (a.first() !is ISeq || !(a.first() as ISeq).seq().any()) TRUE else FALSE
        })),
        Pair(MalSymbol("count"), MalFunction({
            a: ISeq -> if (a.first() is ISeq) MalInteger((a.first() as ISeq).seq().count()) else MalInteger(0)
        })),

        Pair(MalSymbol("="), MalFunction({ a: ISeq -> pairwiseEquals(a) })),
        Pair(MalSymbol("<"), MalFunction({ a: ISeq -> pairwise(a, { x, y -> x.value < y.value }) })),
        Pair(MalSymbol("<="), MalFunction({ a: ISeq -> pairwise(a, { x, y -> x.value <= y.value }) })),
        Pair(MalSymbol(">"), MalFunction({ a: ISeq -> pairwise(a, { x, y -> x.value > y.value }) })),
        Pair(MalSymbol(">="), MalFunction({ a: ISeq -> pairwise(a, { x, y -> x.value >= y.value }) })),

        Pair(MalSymbol("pr-str"), MalFunction({
            a: ISeq -> MalString(a.seq().map({ it -> pr_str(it, print_readably = true) }).joinToString(" "))
        })),
        Pair(MalSymbol("str"), MalFunction({
            a: ISeq -> MalString(a.seq().map({ it -> pr_str(it, print_readably = false) }).joinToString(""))
        })),
        Pair(MalSymbol("prn"), MalFunction({
            a: ISeq -> println(a.seq().map({ it -> pr_str(it, print_readably = true) }).joinToString(" ")); NIL
        })),
        Pair(MalSymbol("println"), MalFunction({
            a: ISeq -> println(a.seq().map({ it -> pr_str(it, print_readably = false) }).joinToString(" ")); NIL
        })),

        Pair(MalSymbol("read-string"), MalFunction({ a: ISeq ->
            val string = a.first() as? MalString ?: throw MalException("slurp requires a string parameter")
            read_str(string.value)
        })),
        Pair(MalSymbol("slurp"), MalFunction({ a: ISeq ->
            val name = a.first() as? MalString ?: throw MalException("slurp requires a filename parameter")
            val text = File(name.value).readText()
            MalString(text)
        }))
)

fun pairwiseEquals(s: ISeq): MalConstant =
        if (s.seq().zip(s.seq().drop(1)).all({ it -> it.first == it.second })) TRUE else FALSE

fun pairwise(s: ISeq, pred: (MalInteger, MalInteger) -> Boolean): MalConstant =
        if (s.seq().zip(s.seq().drop(1)).all({
            it -> pred(it.first as MalInteger, it.second as MalInteger)
        })) TRUE else FALSE
