REM > env library for mal in BBC BASIC

DEF FNnew_env(outer%, binds%, exprs%)
  LOCAL env%
  env% = FNalloc_environment(outer%)
  WHILE NOT FNis_empty(binds%)
    IF FNunbox_symbol(FNfirst(binds%)) = "&" THEN
      PROCenv_set(env%, FNnth(binds%, 1), FNas_list(exprs%))
      binds% = FNempty
    ELSE
      PROCenv_set(env%, FNfirst(binds%), FNfirst(exprs%))
      binds% = FNrest(binds%) : exprs% = FNrest(exprs%)
    ENDIF
  ENDWHILE
=env%

DEF PROCenv_set(env%, keysym%, val%)
  LOCAL data%
  data% = FNenvironment_data(env%)
  data% = FNhashmap_set(data%, FNunbox_symbol(keysym%), val%)
  PROCenvironment_set_data(env%, data%)
ENDPROC

DEF FNenv_find(env%, keysym%)
  LOCAL val%, outer%, key$
  key$ = FNunbox_symbol(keysym%)
  WHILE NOT FNis_nil(env%)
    IF FNhashmap_contains(FNenvironment_data(env%), key$) THEN =env%
    env% = FNenvironment_outer(env%)
  ENDWHILE
=FNnil

DEF FNenv_get(env%, keysym%)
  LOCAL key$
  env% = FNenv_find(env%, keysym%)
  key$ = FNunbox_symbol(keysym%)
  IF FNis_nil(env%) THEN ERROR &40E80922, "'"+key$+"' not found"
=FNhashmap_get(FNenvironment_data(env%), key$)

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
