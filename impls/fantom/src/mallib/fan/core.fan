class Core
{
  static private MalVal prn(MalVal[] a)
  {
    echo(a.join(" ") { it.toString(true) })
    return MalNil.INSTANCE
  }

  static private MalVal println(MalVal[] a)
  {
    echo(a.join(" ") { it.toString(false) })
    return MalNil.INSTANCE
  }

  static private MalVal readline(MalVal[] a)
  {
    line := Env.cur.prompt((a[0] as MalString).value)
    return line == null ? MalNil.INSTANCE : MalString.make(line)
  }

  static private MalVal concat(MalVal[] a)
  {
    return MalList(a.reduce(MalVal[,]) |MalVal[] r, MalSeq v -> MalVal[]| { return r.addAll(v.value) })
  }

  static private MalVal apply(MalVal[] a)
  {
    f := a[0] as MalFunc
    args := a[1..-2]
    args.addAll(((MalSeq)a[-1]).value)
    return f.call(args)
  }

  static private MalVal swap_bang(MalVal[] a)
  {
    atom := a[0] as MalAtom
    MalVal[] args := [atom.value]
    args.addAll(a[2..-1])
    f := a[1] as MalFunc
    return atom.set(f.call(args))
  }

  static Str:MalFunc ns()
  {
    return [
      "=":           MalFunc { MalTypes.toMalBool(it[0] == it[1]) },
      "throw":       MalFunc { throw MalException(it[0]) },

      "nil?":        MalFunc { MalTypes.toMalBool(it[0] is MalNil) },
      "true?":       MalFunc { MalTypes.toMalBool(it[0] is MalTrue) },
      "false?":      MalFunc { MalTypes.toMalBool(it[0] is MalFalse) },
      "string?":     MalFunc { MalTypes.toMalBool(it[0] is MalString && !((MalString)it[0]).isKeyword) },
      "symbol":      MalFunc { MalSymbol.makeFromVal(it[0]) },
      "symbol?":     MalFunc { MalTypes.toMalBool(it[0] is MalSymbol) },
      "keyword":     MalFunc { MalString.makeKeyword((it[0] as MalString).value) },
      "keyword?":    MalFunc { MalTypes.toMalBool(it[0] is MalString && ((MalString)it[0]).isKeyword) },
      "number?":     MalFunc { MalTypes.toMalBool(it[0] is MalInteger) },
      "fn?":         MalFunc { MalTypes.toMalBool(it[0] is MalFunc && !((it[0] as MalUserFunc)?->isMacro ?: false)) },
      "macro?":      MalFunc { MalTypes.toMalBool(it[0] is MalUserFunc && ((MalUserFunc)it[0]).isMacro) },

      "pr-str":      MalFunc { MalString.make(it.join(" ") |MalVal e -> Str| { e.toString(true) }) },
      "str":         MalFunc { MalString.make(it.join("") |MalVal e -> Str| { e.toString(false) }) },
      "prn":         MalFunc(#prn.func),
      "println":     MalFunc(#println.func),
      "read-string": MalFunc { Reader.read_str((it[0] as MalString).value) },
      "readline":    MalFunc(#readline.func),
      "slurp":       MalFunc { MalString.make(File((it[0] as MalString).value.toUri).readAllStr) },

      "<":           MalFunc { MalTypes.toMalBool((it[0] as MalInteger).value < (it[1] as MalInteger).value) },
      "<=":          MalFunc { MalTypes.toMalBool((it[0] as MalInteger).value <= (it[1] as MalInteger).value) },
      ">":           MalFunc { MalTypes.toMalBool((it[0] as MalInteger).value > (it[1] as MalInteger).value) },
      ">=":          MalFunc { MalTypes.toMalBool((it[0] as MalInteger).value >= (it[1] as MalInteger).value) },
      "+":           MalFunc { MalInteger((it[0] as MalInteger).value + (it[1] as MalInteger).value) },
      "-":           MalFunc { MalInteger((it[0] as MalInteger).value - (it[1] as MalInteger).value) },
      "*":           MalFunc { MalInteger((it[0] as MalInteger).value * (it[1] as MalInteger).value) },
      "/":           MalFunc { MalInteger((it[0] as MalInteger).value / (it[1] as MalInteger).value) },
      "time-ms":     MalFunc { MalInteger(DateTime.nowTicks / 1000000) },

      "list":        MalFunc { MalList(it) },
      "list?":       MalFunc { MalTypes.toMalBool(it[0] is MalList) },
      "vector":      MalFunc { MalVector(it) },
      "vector?":     MalFunc { MalTypes.toMalBool(it[0] is MalVector) },
      "hash-map":    MalFunc { MalHashMap.fromList(it) },
      "map?":        MalFunc { MalTypes.toMalBool(it[0] is MalHashMap) },
      "assoc":       MalFunc { (it[0] as MalHashMap).assoc(it[1..-1]) },
      "dissoc":      MalFunc { (it[0] as MalHashMap).dissoc(it[1..-1]) },
      "get":         MalFunc { it[0] is MalNil ? MalNil.INSTANCE : (it[0] as MalHashMap).get2((MalString)it[1], MalNil.INSTANCE) },
      "contains?":   MalFunc { MalTypes.toMalBool((it[0] as MalHashMap).containsKey((MalString)it[1])) },
      "keys":        MalFunc { MalList((it[0] as MalHashMap).keys) },
      "vals":        MalFunc { MalList((it[0] as MalHashMap).vals) },

      "sequential?": MalFunc { MalTypes.toMalBool(it[0] is MalSeq) },
      "cons":        MalFunc { MalList([it[0]].addAll((it[1] as MalSeq).value)) },
      "concat":      MalFunc(#concat.func),
      "nth":         MalFunc { (it[0] as MalSeq).nth((it[1] as MalInteger).value) },
      "first":       MalFunc { (it[0] as MalSeq)?.first ?: MalNil.INSTANCE },
      "rest":        MalFunc { (it[0] as MalSeq)?.rest ?: MalList([,]) },
      "empty?":      MalFunc { MalTypes.toMalBool((it[0] as MalSeq).isEmpty) },
      "count":       MalFunc { MalInteger(it[0].count) },
      "apply":       MalFunc(#apply.func),
      "map":         MalFunc { (it[1] as MalSeq).map(it[0]) },

      "conj":        MalFunc { (it[0] as MalSeq).conj(it[1..-1]) },
      "seq":         MalFunc { it[0].seq },

      "meta":        MalFunc { it[0].meta() },
      "with-meta":   MalFunc { it[0].with_meta(it[1]) },
      "atom":        MalFunc { MalAtom(it[0]) },
      "atom?":       MalFunc { MalTypes.toMalBool(it[0] is MalAtom) },
      "deref":       MalFunc { (it[0] as MalAtom).value },
      "reset!":      MalFunc { (it[0] as MalAtom).set(it[1]) },
      "swap!":       MalFunc(#swap_bang.func),

      "fantom-eval": MalFunc { Interop.fantomEvaluate((it[0] as MalString).value) }
    ]
  }
}
