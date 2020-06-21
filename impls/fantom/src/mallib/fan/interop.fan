using compiler

internal class Interop
{
  static Pod? compile(Str innerBody)
  {
    ci := CompilerInput
    {
      podName     = "mal_fantom_interop_${DateTime.nowUnique}"
      summary     = ""
      isScript    = true
      version     = Version.defVal
      log.level   = LogLevel.silent
      output      = CompilerOutputMode.transientPod
      mode        = CompilerInputMode.str
      srcStr      = "class InteropDummyClass {\nstatic Obj? _evalfunc() {\n $innerBody \n}\n}"
      srcStrLoc   = Loc("mal_fantom_interop")
    }
    try
      return Compiler(ci).compile.transientPod
    catch (CompilerErr e)
      return null
  }

  static Obj? evaluate(Str line)
  {
    p := compile(line)
    if (p == null)
      p = compile("return $line")
    if (p == null)
      p = compile("$line\nreturn null")
    if (p == null)
      return null
    method := p.types.first.method("_evalfunc")
    try
      return method.call()
    catch (Err e)
      return null
  }

  static MalVal fantomToMal(Obj? obj)
  {
    if (obj == null)
      return MalNil.INSTANCE
    else if (obj is Bool)
      return MalTypes.toMalBool((Bool)obj)
    else if (obj is Int)
      return MalInteger((Int)obj)
    else if (obj is List)
      return MalList((obj as List).map |Obj? e -> MalVal| { fantomToMal(e) })
    else if (obj is Map)
    {
      m := [Str:MalVal][:]
      (obj as Map).each |v, k| { m.set(k.toStr, fantomToMal(v)) }
      return MalHashMap.fromMap(m)
    }
    else
      return MalString.make(obj.toStr)
  }

  static MalVal fantomEvaluate(Str line)
  {
    return fantomToMal(evaluate(line))
  }
}
