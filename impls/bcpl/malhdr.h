GLOBAL { readline: ug
         pr_str; throwf
         read_str
         init_types; nil; empty; empty_hashmap; mtrue; mfalse
	 cons; nth; as_lst
	 alloc_int
	 str_setlen; alloc_str; str_bcpl2mal; as_sym; str_eq_const
	 hm_set; hm_contains; hm_get
	 alloc_vec
	 alloc_fun
	 throw
	 catch_level; catch_label; last_exception
	 init_core
}

MANIFEST
{
  // The first word of any mal value indicates its type and suchlike.
  // The "supertype" indicates the meaning of the other words of the
  // value.  The "type" distinguishes mal types with the same supertype
  // (for instance functions and macros).  The compoundflag is set on
  // compund types (ones containing references to other values).
  compoundflag = SLCT 1:7:0; supertype = SLCT 4:0:0; type = SLCT 8:0:0

  // Nil. There is a single nil value initialised by init_types(), but
  // it's a valid pointer so it can safely be dereferenced.
  t_nil = #x00

  // Lists.  These are implemented as a linked list.  The empty list,
  // like nil, is a special value.
  t_lst = #x08; lst_first = 1; lst_rest = 2; lst_sz = 3

  // Integers.
  t_int = #x01; int_value = 1; int_sz = 2

  // Booleans.
  t_boo = #x11

  // Strings.  Unlike conventional BCPL strings, these have an entire word
  // to store the length.  For compatibility with library routines, the
  // first byte of the string is also the length if it will fit.
  t_str = #x02; str_len = 1; str_data = 2

  // Symbols.  Symbols are like strings, but with a different type.
  t_sym = #x12

  maxbcplstrlen = (1 << (BITSPERBCPLWORD / bytesperword)) - 1

  // Vectors.  Structured like strings except that the data consists of
  // pointers rather than packed characters.
  t_vec = #x09; vec_len = 1; vec_data = 2

  // Functions.  Contains a function which gets passed a pointer to
  // this structure and can do what it likes with it.
  t_fun = #x03; fun_code = 1
  // arith() expects the next word to be a function.
  fun_wrapped = 2

  // Hash-maps.  These are implemented as crit-bit trees.  There are three
  // types of node: internal nodes point to two other nodes and encode a
  // bit offset in the spare bits of the first word.  External nodes
  // point to a key and a value.  Empty nodes describe an empty hash-map.
  t_hmi = #x0a; hmi_left = 1; hmi_right = 2; hmi_sz = 3
  hmi_critbit = SLCT 0:8:0; hmi_maxcritbit = (1 << BITSPERBCPLWORD - 8) - 1
  t_hmx = #x1a; hmx_key = 1; hmx_value = 2; hmx_sz = 3
  t_hm0 = #x0b; hm0_sz = 1
}
