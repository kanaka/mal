mixin MalVal
{
  virtual Str toString(Bool readable) { return toStr }
  virtual Int count() { throw Err("count not implemented") }
  virtual MalVal seq() { throw Err("seq not implemented") }
  abstract MalVal meta()
  abstract MalVal with_meta(MalVal newMeta)
}

const mixin MalValNoMeta : MalVal
{
  override MalVal meta() { return MalNil.INSTANCE }
  override MalVal with_meta(MalVal newMeta) { return this }
}

const mixin MalFalseyVal
{
}

const class MalNil : MalValNoMeta, MalFalseyVal
{
  static const MalNil INSTANCE := MalNil()
  override Bool equals(Obj? that) { return that is MalNil }
  override Str toString(Bool readable) { return "nil" }
  override Int count() { return 0 }
  override MalVal seq() { return this }
}

const class MalTrue : MalValNoMeta
{
  static const MalTrue INSTANCE := MalTrue()
  override Bool equals(Obj? that) { return that is MalTrue }
  override Str toString(Bool readable) { return "true" }
}

const class MalFalse : MalValNoMeta, MalFalseyVal
{
  static const MalFalse INSTANCE := MalFalse()
  override Bool equals(Obj? that) { return that is MalFalse }
  override Str toString(Bool readable) { return "false" }
}

const class MalInteger : MalValNoMeta
{
  const Int value
  new make(Int v) { value = v }
  override Bool equals(Obj? that) { return that is MalInteger && (that as MalInteger).value == value }
  override Str toString(Bool readable) { return value.toStr }
}

abstract class MalValBase : MalVal
{
  private MalVal? metaVal := null
  override Str toString(Bool readable) { return toStr }
  override Int count() { throw Err("count not implemented") }
  override MalVal seq() { throw Err("seq not implemented") }
  abstract This dup()
  override MalVal meta() { return metaVal ?: MalNil.INSTANCE }
  override MalVal with_meta(MalVal newMeta)
  {
    v := dup
    v.metaVal = newMeta
    return v
  }
}

class MalSymbol : MalValBase
{
  const Str value
  new make(Str v) { value = v }
  new makeFromVal(MalVal v)
  {
    if (v is MalSymbol) return v
    value = (v as MalString).value
  }
  override Bool equals(Obj? that) { return that is MalSymbol && (that as MalSymbol).value == value }
  override Str toString(Bool readable) { return value }
  override This dup() { return make(value) }
}

class MalString : MalValBase
{
  const Str value
  new make(Str v) { value = v }
  new makeKeyword(Str v) { value = v[0] == '\u029e' ? v : "\u029e$v" }
  override Bool equals(Obj? that) { return that is MalString && (that as MalString).value == value }
  override Str toString(Bool readable)
  {
    if (isKeyword) return ":${value[1..-1]}"
    if (readable)
      return "\"${escapeStr(value)}\""
    else
      return value
  }
  Bool isKeyword() { return !value.isEmpty && value[0] == '\u029e' }
  static Str escapeStr(Str s)
  {
    return s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")
  }
  override MalVal seq()
  {
    if (value.size == 0) return MalNil.INSTANCE
    return MalList(value.chars.map |Int c -> MalString| { MalString.make(Str.fromChars([c])) })
  }
  override This dup() { return make(value) }
}

abstract class MalSeq : MalValBase
{
  MalVal[] value { protected set }
  new make(MalVal[] v) { value = v.ro }
  override Bool equals(Obj? that) { return that is MalSeq && (that as MalSeq).value == value }
  Bool isEmpty() { return value.isEmpty }
  override Int count() { return value.size }
  @Operator MalVal get(Int index) { return value[index] }
  @Operator MalVal[] getRange(Range range) { return value[range] }
  protected Str serialize(Bool readable) { return value.join(" ") { it.toString(readable) } }
  abstract MalSeq drop(Int n)
  MalVal nth(Int index) { return index < count ? get(index) : throw Err("nth: index out of range") }
  MalVal first() { return isEmpty ? MalNil.INSTANCE : value[0] }
  MalList rest() { return MalList(isEmpty ? [,] : value[1..-1]) }
  MalList map(MalFunc f) { return MalList(value.map |MalVal v -> MalVal| { f.call([v]) } ) }
  abstract MalSeq conj(MalVal[] args)
}

class MalList : MalSeq
{
  new make(MalVal[] v) : super.make(v) {}
  override Str toString(Bool readable) { return "(${serialize(readable)})" }
  override MalList drop(Int n) { return make(value[n..-1]) }
  override MalVal seq() { return isEmpty ? MalNil.INSTANCE : this }
  override MalList conj(MalVal[] args) { return MalList(value.rw.insertAll(0, args.reverse)) }
  override This dup() { return make(value) }
}

class MalVector : MalSeq
{
  new make(MalVal[] v) : super.make(v) {}
  override Str toString(Bool readable) { return "[${serialize(readable)}]" }
  override MalVector drop(Int n) { return make(value[n..-1]) }
  override MalVal seq() { return isEmpty ? MalNil.INSTANCE : MalList(value) }
  override MalVector conj(MalVal[] args) { return MalVector(value.rw.addAll(args)) }
  override This dup() { return make(value) }
}

class MalHashMap : MalValBase
{
  Str:MalVal value { private set }
  new fromList(MalVal[] lst) {
    m := [Str:MalVal][:]
    for (i := 0; i < lst.size; i += 2)
      m.add((lst[i] as MalString).value, (MalVal)lst[i + 1])
    value = m.ro
  }
  new fromMap(Str:MalVal m) { value = m.ro }
  override Bool equals(Obj? that) { return that is MalHashMap && (that as MalHashMap).value == value }
  override Str toString(Bool readable)
  {
    elements := Str[,]
    value.each(|MalVal v, Str k| { elements.add(MalString.make(k).toString(readable)); elements.add(v.toString(readable)) })
    s := elements.join(" ")
    return "{$s}"
  }
  override Int count() { return value.size }
  @Operator MalVal get(Str key) { return value[key] }
  MalVal get2(MalString key, MalVal? def := null) { return value.get(key.value, def) }
  Bool containsKey(MalString key) { return value.containsKey(key.value) }
  MalVal[] keys() { return value.keys.map |Str k -> MalVal| { MalString.make(k) } }
  MalVal[] vals() { return value.vals }
  MalHashMap assoc(MalVal[] args)
  {
    newValue := value.dup
    for (i := 0; i < args.size; i += 2)
      newValue.set((args[i] as MalString).value, args[i + 1])
    return fromMap(newValue)
  }
  MalHashMap dissoc(MalVal[] args)
  {
    newValue := value.dup
    args.each { newValue.remove((it as MalString).value) }
    return fromMap(newValue)
  }
  override This dup() { return fromMap(value) }
}

class MalFunc : MalValBase
{
  protected |MalVal[] a -> MalVal| f
  new make(|MalVal[] a -> MalVal| func) { f = func }
  MalVal call(MalVal[] a) { return f(a) }
  override Str toString(Bool readable) { return "<Function>" }
  override This dup() { return make(f) }
}

class MalUserFunc : MalFunc
{
  MalVal ast { private set }
  private MalEnv env
  private MalSeq params
  Bool isMacro := false
  new make(MalVal ast, MalEnv env, MalSeq params, |MalVal[] a -> MalVal| func, Bool isMacro := false) : super.make(func)
  {
    this.ast = ast
    this.env = env
    this.params = params
    this.isMacro = isMacro
  }
  MalEnv genEnv(MalSeq args) { return MalEnv(env, params, args) }
  override Str toString(Bool readable) { return "<Function:args=${params.toString(readable)}, isMacro=${isMacro}>" }
  override This dup() { return make(ast, env, params, f, isMacro) }
}

class MalAtom : MalValBase
{
  MalVal value
  new make(MalVal v) { value = v }
  override Str toString(Bool readable) { return "(atom ${value.toString(readable)})" }
  override Bool equals(Obj? that) { return that is MalAtom && (that as MalAtom).value == value }
  MalVal set(MalVal v) { value = v; return value }
  override This dup() { return make(value) }
}

class MalTypes
{
  static MalVal toMalBool(Bool cond) { return cond ? MalTrue.INSTANCE : MalFalse.INSTANCE }
  static Bool isPair(MalVal a) { return a is MalSeq && !(a as MalSeq).isEmpty }
}

const class MalException : Err
{
  const Str serializedValue
  new make(MalVal v) : super.make("Mal exception") { serializedValue = v.toString(true) }
  MalVal getValue() { return Reader.read_str(serializedValue) }
}
