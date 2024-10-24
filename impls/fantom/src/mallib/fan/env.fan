class MalEnv
{
  private Str:MalVal data := [:]
  private MalEnv? outer

  new make(MalEnv? outer := null, MalSeq? binds := null, MalSeq? exprs := null)
  {
    this.outer = outer
    if (binds != null && exprs != null)
    {
      for (i := 0; i < binds.count; i++)
      {
        if ((binds[i] as MalSymbol).value == "&")
        {
          set(binds[i + 1], MalList(exprs[i..-1]))
          break
        }
        else
          set(binds[i], exprs[i])
      }
    }
  }

  MalVal set(MalSymbol key, MalVal value)
  {
    data[key.value] = value
    return value
  }

  MalVal? get(Str key)
  {
    return data.containsKey(key) ? data[key] : outer?.get(key)
  }
}
