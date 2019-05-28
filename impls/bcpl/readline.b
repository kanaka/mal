GET "libhdr"
GET "malhdr"

MANIFEST
{ buflen = (1 << bitsperbyte) - 1 }

// readline returns a newly-allocated mal string.
// 'prompt' is a mal string,
LET readline(prompt) = VALOF
{ LET buf = VEC 1 + buflen / bytesperword
  LET p = 1
  LET ch = 0
  writes(@prompt!str_data)
  deplete(cos)
  { IF p > buflen THEN { writes("Input line too long"); FINISH }
    ch := rdch()
    buf%p := ch
    p := p + 1
  } REPEATUNTIL ch = '*n'
  buf%0 := p - 1
  RESULTIS str_bcpl2mal(buf)
}
