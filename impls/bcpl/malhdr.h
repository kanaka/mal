GLOBAL { readline: ug
         pr_str; pr_multi; throwf
         read_str
         init_types; nil; empty; empty_hashmap; mtrue; mfalse
	 gc_mark; gc_sweep
	 equal
	 cons; nth; as_lst
	 alloc_int
	 str_setlen; alloc_str; str_bcpl2mal; as_sym; as_kwd; str_eq_const
	 hm_set; hm_contains; hm_get
	 alloc_vec
	 alloc_fun
	 throw
	 env_new; env_set
	 catch_level; catch_label; last_exception
	 core_env
}

MANIFEST
{
  // The first word of each value is a pointer to the next element
  // of the global object list.
  nextptr = 0

  // The second word of any mal value indicates its type and suchlike.
  // The "supertype" indicates the meaning of the other words of the
  // value.  The "type" distinguishes mal types with the same supertype
  // (for instance functions and macros).  The compoundflag is set on
  // compund types (ones containing references to other values).
  compoundflag = SLCT 1:3:1; supertype = SLCT 4:0:1; type = SLCT 8:0:1
  gc_marked = SLCT 1:8:1

  // Nil. There is a single nil value initialised by init_types(), but
  // it's a valid pointer so it can safely be dereferenced.
  t_nil = #x00
  nil_sz = 2

  // Lists.  These are implemented as a linked list.  The empty list,
  // like nil, is a special value.
  t_lst = #x08; lst_first = 2; lst_rest = 3; lst_sz = 4

  // Integers.
  t_int = #x01; int_value = 2; int_sz = 3

  // Booleans.
  t_boo = #x11

  // Strings.  Unlike conventional BCPL strings, these have an entire word
  // to store the length.  For compatibility with library routines, the
  // first byte of the string is also the length if it will fit.
  t_str = #x02; str_len = 2; str_data = 3

  // Symbols and keywords.  Like strings, but with different types.
  t_sym = #x12
  t_kwd = #x22

  maxbcplstrlen = (1 << (BITSPERBCPLWORD / bytesperword)) - 1

  // Vectors.  Structured like strings except that the data consists of
  // pointers rather than packed characters.
  t_vec = #x09; vec_len = 2; vec_data = 3

  // Functions.  Contains a function which gets passed a pointer to
  // this structure and can do what it likes with it.
  t_fun = #x0f; fun_code = 2; fun_data = 3

  // Hash-maps.  These are implemented as crit-bit trees.  There are three
  // types of node: internal nodes point to two other nodes and encode a
  // bit offset in the spare bits of the first word.  External nodes
  // point to a key and a value.  Empty nodes describe an empty hash-map.
  t_hmi = #x0a; hmi_left = 2; hmi_right = 3; hmi_sz = 4
  hmi_critbit = SLCT 0:9:1; hmi_maxcritbit = (1 << BITSPERBCPLWORD - 9) - 1
  t_hmx = #x1a; hmx_key = 2; hmx_value = 3; hmx_sz = 4
  t_hm0 = #x0b; hm0_sz = 2
}
