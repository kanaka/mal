REM > env library for mal in BBC BASIC

DEF FNnew_env(outer%, binds%, exprs%)
  LOCAL env%, key$
  env% = FNalloc_environment(outer%)
  WHILE NOT FNis_empty(binds%)
    key$ = FNunbox_symbol(FNfirst(binds%))
    IF key$ = "&" THEN
      PROCenv_set(env%, FNunbox_symbol(FNnth(binds%, 1)), FNas_list(exprs%))
      binds% = FNempty
    ELSE
      PROCenv_set(env%, key$, FNfirst(exprs%))
      binds% = FNrest(binds%) : exprs% = FNrest(exprs%)
    ENDIF
  ENDWHILE
=env%

DEF PROCenv_set(env%, key$, val%)
  LOCAL data%
  data% = FNenvironment_data(env%)
  data% = FNhashmap_set(data%, key$, val%)
  PROCenvironment_set_data(env%, data%)
ENDPROC

DEF FNenv_find(env%, key$)
  WHILE NOT FNis_nil(env%)
    IF FNhashmap_contains(FNenvironment_data(env%), key$) THEN =env%
    env% = FNenvironment_outer(env%)
  ENDWHILE
=FNnil

DEF FNenv_get(env%, key$)
  env% = FNenv_find(env%, key$)
  IF FNis_nil(env%) THEN ERROR &40E80922, "'"+key$+"' not found"
=FNhashmap_get(FNenvironment_data(env%), key$)

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
