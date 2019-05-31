REM > types library for mal in BBC BASIC

REM  This library should be the only thing that understands the
REM  implementation of mal data types in BBC BASIC.  All other
REM  code should use routines in this library to access them.

REM  As far as other code is concerned, a mal object is just an
REM  opaque 32-bit integer, which might be a pointer, or might not.

REM  All mal objects live in an array, Z%(), with string values held
REM  in a parallel array, Z$().  There's one row in Z%(), and one
REM  entry in Z$(), for each mal object.

REM  Z%(x,0) holds the type of an object and other small amounts of
REM  information.  The bottom bit indicates the semantics of Z%(x,1):

REM  &01 : Z%(x,1) is a pointer into Z%()

REM  Z%(x,2) and Z%(x,3) are always pointers into Z%(), to 'nil' if nothing
REM  else.

REM  The &40 bit is used to distinguish empty lists, vectors and hash-maps.
REM  The &80 bit distinguishes vectors from lists and macros from functions.

REM  sS%() is a shadow stack, used to keep track of which mal values might
REM  be referenced from local variables at a given depth of the BASIC call
REM  stack.  It grows upwards.  sSP% points to the first unused word.  sFP%
REM  points to the start of the current shadow stack frame.  The first word
REM  of each shadow stack frame is the saved value of sFP%.  The rest are
REM  mal values.

REM  Types are:
REM  &00  nil
REM  &04  boolean
REM  &08  integer
REM  &0C  core function
REM  &01  atom
REM  &05  free block
REM  &09  list/vector (each object is a cons cell)
REM  &0D  environment
REM  &11  hash-map internal node
REM  &15  mal function (first part)
REM  &19  mal function (second part)
REM  &02  string/keyword
REM  &06  symbol
REM  &0A  hash-map leaf node

REM  Formats of individual objects are defined below.

DEF PROCtypes_init
  REM  Mal's heap has to be statically dimensioned, but we also
  REM  need to leave enough space for BASIC's stack and heap.
  REM  The BASIC heap is where all strings live.
  REM
  REM  Each row of Z%() consumes 16 bytes.  The size of each entry
  REM  in Z$() varies by platform: 5 bytes in ARM BBC BASIC V,
  REM  8 bytes in Brandy on a 32-bit system, 16 bytes in Brandy on
  REM  a 64-bit system.

  DIM Z%((HIMEM-LOMEM)/110,3), Z$((HIMEM-LOMEM)/110)
  DIM sS%((HIMEM-LOMEM)/64)

  Z%(1,0) = &04 : REM false
  Z%(2,0) = &04 : Z%(2,1) = TRUE : REM true
  Z%(3,0) = &49 : Z%(3,1) = 3 : REM empty list
  Z%(4,0) = &C9 : Z%(4,1) = 4 : REM empty vector
  Z%(5,0) = &51 : REM empty hashmap
  next_Z% = 6
  sSP% = 1
  sFP% = 0
  F% = 0
ENDPROC

DEF FNtype_of(val%)
=Z%(val%,0) AND &1F

DEF PROCgc_enter
  REM PRINT ;sFP%;
  sS%(sSP%) = sFP%
  sFP% = sSP%
  sSP% += 1
  REM PRINT " >>> ";sFP%
ENDPROC

REM  FNgc_save is equivalent to PROCgc_enter except that it returns a
REM  value that can be passed to PROCgc_restore to pop all the stack
REM  frames back to (and including) the one pushed by FNgc_save.
DEF FNgc_save
  PROCgc_enter
=sFP%

DEF PROCgc_exit
  REM PRINT ;sS%(sFP%);" <<< ";sFP%
  sSP% = sFP%
  sFP% = sS%(sFP%)
ENDPROC

DEF PROCgc_restore(oldFP%)
  sFP% = oldFP%
  REM PRINT "!!! FP reset"
  PROCgc_exit
ENDPROC

DEF FNref_local(val%)
  sS%(sSP%) = val%
  sSP% += 1
=val%

DEF FNgc_exit(val%)
  PROCgc_exit
=FNref_local(val%)

DEF FNgc_restore(oldFP%, val%)
  PROCgc_restore(oldFP%)
=FNref_local(val%)

DEF PROCgc_keep_only2(val1%, val2%)
  PROCgc_exit
  PROCgc_enter
  val1% = FNref_local(val1%)
  val2% = FNref_local(val2%)
ENDPROC

DEF FNmalloc(type%)
  LOCAL val%
  REM  If the heap is full, collect garbage first.
  IF F% = 0 AND next_Z% > DIM(Z%(),1) THEN
    PROCgc
    IF F% = 0 ERROR &40E80950, "Out of mal heap memory"
  ENDIF
  IF F% <> 0 THEN
    val% = F%
    F% = Z%(val%,1)
  ELSE
    val% = next_Z%
    next_Z% += 1
  ENDIF
  Z%(val%,0) = type%
=FNref_local(val%)

DEF PROCfree(val%)
  Z%(val%,0) = &05
  Z%(val%,1) = F%
  Z%(val%,2) = 0
  Z%(val%,3) = 0
  Z$(val%) = ""
  F% = val%
ENDPROC

DEF PROCgc
  REM PRINT "** START GC **"
  PROCgc_markall
  PROCgc_sweep
  REM PRINT "** FINISH GC **"
ENDPROC

DEF PROCgc_markall
  LOCAL sp%, fp%
  fp% = sFP%
  REM PRINT ">>marking...";
  FOR sp% = sSP% - 1 TO 0 STEP -1
    IF sp% = fp% THEN
      fp% = sS%(sp%)
      REM PRINT " / ";
    ELSE PROCgc_mark(sS%(sp%))
    ENDIF
  NEXT sp%
  REM PRINT
ENDPROC

DEF PROCgc_mark(val%)
  IF (Z%(val%,0) AND &100) = 0 THEN
    REM PRINT " ";val%;
    Z%(val%,0) += &100
    IF (Z%(val%,0) AND &01) THEN PROCgc_mark(Z%(val%,1))
    PROCgc_mark(Z%(val%,2))
    PROCgc_mark(Z%(val%,3))
  ENDIF
ENDPROC

DEF PROCgc_sweep
  LOCAL val%
  REM PRINT ">>sweeping ...";
  FOR val% = 6 TO next_Z% - 1
    IF FNtype_of(val%) <> &05 AND (Z%(val%,0) AND &100) = 0 THEN
      REM PRINT " ";val%;
      PROCfree(val%)
    ELSE
      Z%(val%,0) -= &100
    ENDIF
  NEXT val%
  REM PRINT
ENDPROC

DEF FNmeta(val%)
=Z%(val%,3)

DEF FNwith_meta(val%, meta%)
  LOCAL newval%
  newval% = FNmalloc(Z%(val%,0))
  Z%(newval%,1) = Z%(val%,1)
  Z%(newval%,2) = Z%(val%,2)
  Z%(newval%,3) = meta%
  Z$(newval%) = Z$(val%)
=newval%

REM ** Nil **

DEF FNis_nil(val%)
=FNtype_of(val%) = 0

DEF FNnil
=0

REM ** Boolean **

REM  Z%(x,1) = TRUE or FALSE

DEF FNis_boolean(val%)
=FNtype_of(val%) = &04

DEF FNalloc_boolean(bval%)
  IF bval% THEN =2
=1

DEF FNunbox_boolean(val%)
  IF NOT FNis_boolean(val%) THEN ERROR &40E80911, "Not a boolean"
=Z%(val%,1)

DEF FNis_truish(val%)
  IF FNis_nil(val%) THEN =FALSE
  IF FNis_boolean(val%) THEN =FNunbox_boolean(val%)
=TRUE

REM ** Integers **

REM  Z%(x,1) = integer value

DEF FNis_int(val%)
=FNtype_of(val%) = &08

DEF FNalloc_int(ival%)
  LOCAL val%
  val% = FNmalloc(&08)
  Z%(val%,1) = ival%
=val%

DEF FNunbox_int(val%)
  IF NOT FNis_int(val%) THEN ERROR &40E80912, "Not an integer"
=Z%(val%,1)

REM ** Strings and keywords **

REM  Z$(x) is the string value
REM  Z%(x,2) points to the next part of the string
REM  A keyword is a string with first character CHR$(127).

DEF FNis_string(val%)
=FNtype_of(val%) = &02

DEF FNalloc_string(sval$)
  LOCAL val%
  val% = FNmalloc(&02)
  Z$(val%) = sval$
=val%

DEF FNunbox_string(val%)
  IF NOT FNis_string(val%) THEN ERROR &40E80914, "Not a string"
  IF NOT FNis_nil(Z%(val%,2)) ERROR &40E80914, "Cannot unbox a long string"
=Z$(val%)

DEF FNstring_append(val%, add$)
  LOCAL newval%
  IF NOT FNis_string(val%) THEN ERROR &40E80914, "Not a string"
  newval% = FNalloc_string(Z$(val%))
  IF FNis_nil(Z%(val%,2)) THEN
    IF LEN(Z$(newval%)) + LEN(add$) <= 255 THEN
      Z$(newval%) += add$
    ELSE
      Z%(newval%,2) = FNalloc_string(add$)
    ENDIF
  ELSE
    Z%(newval%,2) = FNstring_append(Z%(val%,2), add$)
  ENDIF
=newval%

DEF FNstring_concat(val%, add%)
  LOCAL newval%
  IF NOT FNis_string(val%) THEN ERROR &40E80914, "Not a string"
  IF NOT FNis_string(add%) THEN ERROR &40E80914, "Not a string"
  newval% = FNalloc_string(Z$(val%))
  IF FNis_nil(Z%(val%,2)) THEN
    IF LEN(Z$(newval%)) + LEN(Z$(add%)) <= 255 THEN
      Z$(newval%) += Z$(add%)
      Z%(newval%,2) = Z%(add%,2)
    ELSE
      Z%(newval%,2) = add%
    ENDIF
  ELSE
    Z%(newval%,2) = FNstring_concat(Z%(val%,2), add%)
  ENDIF
=newval%

DEF FNstring_len(val%)
  LOCAL len%
  WHILE NOT FNis_nil(val%)
    len% += LEN(Z$(val%))
    val% = Z%(val%,2)
  ENDWHILE
=len%

DEF FNstring_chr(val%, pos%)
  WHILE pos% > LEN(Z$(val%))
    pos% -= LEN(Z$(val%))
    val% = Z%(val%,2)
    IF FNis_nil(val%) THEN =""
  ENDWHILE
=MID$(Z$(val%), pos%, 1)

REM ** Symbols **

REM  Z$(x) = value of the symbol

DEF FNis_symbol(val%)
=FNtype_of(val%) = &06

DEF FNalloc_symbol(sval$)
  LOCAL val%
  val% = FNmalloc(&06)
  Z$(val%) = sval$
=val%

DEF FNunbox_symbol(val%)
  IF NOT FNis_symbol(val%) THEN ERROR &40E80915, "Not a symbol"
=Z$(val%)

REM ** Lists and vectors **

REM  Lists and vectors are both represented as linked lists: the only
REM  difference is in the state of the is_vector flag in the head cell
REM  of the list.  Note that this means that the tail of a list may be
REM  a vector, and vice versa.  FNas_list and FNas_vector can be used
REM  to convert a sequence to a particular type as necessary.

REM  Z%(x,0) AND &80 = is_vector flag
REM  Z%(x,1) = index in Z%() of next pair
REM  Z%(x,2) = index in Z%() of first element

REM  The empty list is a distinguished value, with elements that match
REM  the spec of 'first' and 'rest'.

DEF FNempty
=3

DEF FNempty_vector
=4

DEF FNalloc_pair(car%, cdr%)
  LOCAL val%
  val% = FNmalloc(&09)
  Z%(val%,2) = car%
  Z%(val%,1) = cdr%
=val%

DEF FNalloc_vector_pair(car%, cdr%)
  LOCAL val%
  val% = FNalloc_pair(car%, cdr%)
  Z%(val%,0) = Z%(val%,0) OR &80
=val%

DEF FNis_empty(val%)
=(Z%(val%,0) AND &40) = &40

DEF FNis_seq(val%)
=FNtype_of(val%) = &09

DEF FNis_list(val%)
=FNtype_of(val%) = &09 AND (Z%(val%, 0) AND &80) = &00

DEF FNis_vector(val%)
=FNtype_of(val%) = &09 AND (Z%(val%, 0) AND &80) = &80

DEF FNas_list(val%)
  IF FNis_list(val%) THEN =val%
  IF FNis_empty(val%) THEN =FNempty
=FNalloc_pair(FNfirst(val%), FNrest(val%))

DEF FNas_vector(val%)
  IF FNis_vector(val%) THEN =val%
  IF FNis_empty(val%) THEN =FNempty_vector
=FNalloc_vector_pair(FNfirst(val%), FNrest(val%))

DEF FNfirst(val%)
  IF NOT FNis_seq(val%) THEN ERROR &40E80916, "Can't get car of non-sequence"
=FNref_local(Z%(val%,2))

DEF FNrest(val%)
  IF NOT FNis_seq(val%) THEN ERROR &40E80916, "Can't get cdr of non-sequence"
=FNref_local(Z%(val%,1))

DEF FNalloc_list2(val0%, val1%)
  =FNalloc_pair(val0%, FNalloc_pair(val1%, FNempty))

DEF FNalloc_list3(val0%, val1%, val2%)
  =FNalloc_pair(val0%, FNalloc_pair(val1%, FNalloc_pair(val2%, FNempty)))

DEF FNcount(val%)
  LOCAL i%
  WHILE NOT FNis_empty(val%)
    val% = FNrest(val%)
    i% += 1
  ENDWHILE
= i%

DEF FNnth(val%, n%)
  WHILE n% > 0
    IF FNis_empty(val%) THEN ERROR &40E80923, "Subscript out of range"
    val% = FNrest(val%)
    n% -= 1
  ENDWHILE
  IF FNis_empty(val%) THEN ERROR &40E80923, "Subscript out of range"
=FNfirst(val%)

REM ** Core functions **

REM  Z%(x,1) = index of function in FNcore_call

DEF FNis_corefn(val%)
=FNtype_of(val%) = &0C

DEF FNalloc_corefn(fn%)
  LOCAL val%
  val% = FNmalloc(&0C)
  Z%(val%,1) = fn%
=val%

DEF FNunbox_corefn(val%)
  IF NOT FNis_corefn(val%) THEN ERROR &40E80919, "Not a core function"
=Z%(val%,1)

REM ** Hash-maps **

REM  Hash-maps are represented as a crit-bit tree.

REM  An internal node has:
REM  Z%(x,0) >> 16 = next bit of key to check
REM  Z%(x,1) = index in Z%() of left child (if next bit of key is 0)
REM  Z%(x,2) = index in Z%() of right child (if next bit of key is 1)

REM  A leaf node has
REM  Z$(x) = key
REM  Z%(x,2) = index in Z%() of value

REM  The empty hash-map is a special value containing no data.

DEF FNempty_hashmap
=5

DEF FNhashmap_alloc_leaf(key$, val%)
  LOCAL entry%
  entry% = FNmalloc(&0A)
  Z$(entry%) = key$
  Z%(entry%,2) = val%
=entry%  

DEF FNhashmap_alloc_node(bit%, left%, right%)
  LOCAL entry%
  entry% = FNmalloc(&11)
  Z%(entry%,0) += (bit% << 16)
  Z%(entry%,1) = left%
  Z%(entry%,2) = right%
=entry%  

DEF FNis_hashmap(val%)
  LOCAL t%
  t% = FNtype_of(val%)
=t% = &11 OR t% = &0A

DEF FNkey_bit(key$, bit%)
  LOCAL cnum%
  cnum% = bit% >> 3
  IF cnum% >= LEN(key$) THEN =FALSE
=ASC(MID$(key$, cnum% + 1, 1)) AND (&80 >> (bit% AND 7))  

DEF FNkey_bitdiff(key1$, key2$)
  LOCAL bit%
  WHILE FNkey_bit(key1$, bit%) = FNkey_bit(key2$, bit%)
    bit% += 1
  ENDWHILE
=bit%

DEF FNhashmap_set(map%, key$, val%)
  LOCAL bit%, nearest%
  IF FNis_empty(map%) THEN =FNhashmap_alloc_leaf(key$, val%)
  nearest% = FNhashmap_find(map%, key$)
  IF Z$(nearest%) = key$ THEN =FNhashmap_replace(map%, key$, val%)
  bit% = FNkey_bitdiff(key$, Z$(nearest%))
=FNhashmap_insert(map%, bit%, key$, val%)

DEF FNhashmap_insert(map%, bit%, key$, val%)
  LOCAL left%, right%
  IF FNtype_of(map%) = &11 AND (Z%(map%,0) >> 16) < bit% THEN
    IF FNkey_bit(key$, Z%(map%,0) >> 16) THEN
      left% = Z%(map%,1)
      right% = FNhashmap_insert(Z%(map%,2), bit%, key$, val%)
    ELSE
      left% = FNhashmap_insert(Z%(map%,1), bit%, key$, val%)
      right% = Z%(map%,2)
    ENDIF
    =FNhashmap_alloc_node(Z%(map%,0)>>16, left%, right%)
  ENDIF
  IF FNkey_bit(key$, bit%) THEN
    left% = map%
    right% = FNhashmap_alloc_leaf(key$, val%)
  ELSE
    left% = FNhashmap_alloc_leaf(key$, val%)
    right% = map%
  ENDIF
=FNhashmap_alloc_node(bit%, left%, right%)


REM  Replace a known-present key in a non-empty hashmap.
DEF FNhashmap_replace(map%, key$, val%)
  LOCAL left%, right%
  IF FNtype_of(map%) = &0A THEN =FNhashmap_alloc_leaf(key$, val%)
  IF FNkey_bit(key$, Z%(map%,0) >> 16) THEN
    left% = Z%(map%,1)
    right% = FNhashmap_replace(Z%(map%,2), key$, val%)
  ELSE
    left% = FNhashmap_replace(Z%(map%,1), key$, val%)
    right% = Z%(map%,2)
  ENDIF
=FNhashmap_alloc_node(Z%(map%,0)>>16, left%, right%)

DEF FNhashmap_remove(map%, key$)
  LOCAL child%
  IF FNis_empty(map%) THEN =map%
  IF FNtype_of(map%) = &0A THEN
    IF Z$(map%) = key$ THEN =FNempty_hashmap
  ENDIF
  IF FNkey_bit(key$, Z%(map%,0) >> 16) THEN
    child% = FNhashmap_remove(Z%(map%,2), key$)
    IF FNis_empty(child%) THEN =Z%(map%,1)
    =FNhashmap_alloc_node(Z%(map%,0)>>16, Z%(map%,1), child%)
  ELSE
    child% = FNhashmap_remove(Z%(map%,1), key$)
    IF FNis_empty(child%) THEN =Z%(map%,2)
    =FNhashmap_alloc_node(Z%(map%,0)>>16, child%, Z%(map%,2))
  ENDIF

REM  FNhashmap_find finds the nearest entry in a non-empty hash-map to
REM  the key requested, and returns the entire entry.
DEF FNhashmap_find(map%, key$)
  WHILE FNtype_of(map%) = &11
    IF FNkey_bit(key$, Z%(map%,0) >> 16) THEN map% = Z%(map%,2) ELSE map% = Z%(map%,1)
  ENDWHILE
=map%

DEF FNhashmap_get(map%, key$)
  IF NOT FNis_hashmap(map%) THEN ERROR &40E80918, "Can't get item from a non-hashmap"
  IF FNis_empty(map%) THEN =FNnil
  map% = FNhashmap_find(map%, key$)
IF Z$(map%) = key$ THEN =FNref_local(Z%(map%,2)) ELSE =FNnil

DEF FNhashmap_contains(map%, key$)
  IF NOT FNis_hashmap(map%) THEN ERROR &40E80918, "Can't get item from a non-hashmap"
  IF FNis_empty(map%) THEN =FALSE
  map% = FNhashmap_find(map%, key$)
=Z$(map%) = key$

DEF FNhashmap_keys(map%)
=FNhashmap_keys1(map%, FNempty)

DEF FNhashmap_keys1(map%, acc%)
  IF FNis_empty(map%) THEN =acc%
  IF FNtype_of(map%) = &0A THEN
    =FNalloc_pair(FNalloc_string(Z$(map%)), acc%)
  ENDIF
=FNhashmap_keys1(Z%(map%,1), FNhashmap_keys1(Z%(map%,2), acc%))

DEF FNhashmap_vals(map%)
=FNhashmap_vals1(map%, FNempty)

DEF FNhashmap_vals1(map%, acc%)
  IF FNis_empty(map%) THEN =acc%
  IF FNtype_of(map%) = &0A THEN
    =FNalloc_pair(Z%(map%,2), acc%)
  ENDIF
=FNhashmap_vals1(Z%(map%,1), FNhashmap_vals1(Z%(map%,2), acc%))

DEF PROChashmap_dump(map%)
  IF FNis_empty(map%) THEN
    PRINT "[empty]"
  ELSE
    PRINT "[-----]"
    PROChashmap_dump_internal(map%, "")
  ENDIF
ENDPROC

DEF PROChashmap_dump_internal(map%, prefix$)
  IF FNtype_of(map%) = &0A PRINT prefix$;Z$(map%)
  IF FNtype_of(map%) = &11 THEN
    PRINT prefix$;"<";Z%(map%,0) >> 16;">"
    PROChashmap_dump_internal(Z%(map%,1), prefix$ + "L ")
    PROChashmap_dump_internal(Z%(map%,2), prefix$ + "R ")
  ENDIF
ENDPROC

REM ** Functions **

REM  A function is represented by two cells:
REM  Z%(x,0) AND &80 = is_macro flag
REM  Z%(x,1) = index in Z%() of ast
REM  Z%(x,2) = y

REM  Z%(y,1) = index in Z%() of params
REM  Z%(y,2) = index in Z%() of env

DEF FNis_fn(val%)
=FNtype_of(val%) = &15

DEF FNis_nonmacro_fn(val%)
=FNtype_of(val%) = &15 AND (Z%(val%, 0) AND &80) = &00

DEF FNis_macro(val%)
=FNtype_of(val%) = &15 AND (Z%(val%, 0) AND &80) = &80

DEF FNalloc_fn(ast%, params%, env%)
  LOCAL val1%, val2%
  val1% = FNmalloc(&15)
  Z%(val1%,1) = ast%
  val2% = FNmalloc(&19)
  Z%(val1%,2) = val2%
  Z%(val2%,1) = params%
  Z%(val2%,2) = env%
=val1%

DEF FNas_macro(val%)
  IF NOT FNis_fn(val%) THEN ERROR &40E8091A, "Not a function"
  LOCAL newval%
  newval% = FNmalloc(Z%(val%,0) OR &80)
  Z%(newval%,1) = Z%(val%,1)
  Z%(newval%,2) = Z%(val%,2)
  Z%(newval%,3) = Z%(val%,3)
=newval%

DEF FNfn_ast(val%)
  IF NOT FNis_fn(val%) THEN ERROR &40E8091A, "Not a function"
=FNref_local(Z%(val%,1))

DEF FNfn_params(val%)
  IF NOT FNis_fn(val%) THEN ERROR &40E8091A, "Not a function"
=FNref_local(Z%(Z%(val%,2),1))

DEF FNfn_env(val%)
  IF NOT FNis_fn(val%) THEN ERROR &40E8091A, "Not a function"
=FNref_local(Z%(Z%(val%,2),2))

REM ** Atoms **

REM Z%(x,1) = index in Z% of current referent

DEF FNis_atom(val%)
=FNtype_of(val%) = &01

DEF FNalloc_atom(contents%)
  LOCAL val%
  val% = FNmalloc(&01)
  Z%(val%,1) = contents%
=val%

DEF FNatom_deref(val%)
=FNref_local(Z%(val%,1))

DEF PROCatom_reset(val%, contents%)
  Z%(val%,1) = contents%
ENDPROC

REM ** Environments **

REM  Z%(x,1) = index in Z% of hash-map
REM  Z%(x,2) = index in Z% of outer environment

DEF FNis_environment(val%)
=FNtype_of(val%) = &0D

DEF FNalloc_environment(outer%)
  LOCAL val%
  val% = FNmalloc(&0D)
  Z%(val%,1) = FNempty_hashmap
  Z%(val%,2) = outer%
=val%

DEF FNenvironment_data(val%)
  IF NOT FNis_environment(val%) THEN ERROR &40E8091D, "Not an environment"
=FNref_local(Z%(val%,1))

DEF PROCenvironment_set_data(val%, data%)
  IF NOT FNis_environment(val%) THEN ERROR &40E8091D, "Not an environment"
  Z%(val%,1) = data%
ENDPROC

DEF FNenvironment_outer(val%)
  IF NOT FNis_environment(val%) THEN ERROR &40E8091D, "Not an environment"
=FNref_local(Z%(val%,2))

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
