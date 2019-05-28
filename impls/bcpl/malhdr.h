GLOBAL { readline: ug; alloc_str; str_bcpl2mal }

MANIFEST
{
  // The first word of any mal value indicates its type and suchlike.
  // The "supertype" indicates the meaning of the other words of the
  // value.  The "type" distinguishes mal types with the same supertype
  // (for instance functions and macros).
  type = SLCT 8; supertype = SLCT 4

  // Strings.  Unlike conventional BCPL strings, these have an entire word
  // to store the length.  For compatibility with library routines, the
  // first byte of the string is also the length if it will fit.
  t_str = #x00; str_len = 1; str_data = 2
}
