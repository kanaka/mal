GET "libhdr"
GET "malhdr"

MANIFEST
{ buflen = (1 << bitsperbyte) - 1 }

// readline returns a newly-allocated mal string.
// 'prompt' is a mal string,
LET readline(prompt) = VALOF
{ LET dest, dest_size, ptr = ?, 1024, 1
  LET ch = 0
  writes(@prompt!str_data)
  deplete(cos)
  dest := alloc_str(dest_size)
  { LET c = rdch()
    IF c = endstreamch RESULTIS nil
    IF c = '*n' BREAK
    IF ptr >= dest_size THEN
    { LET tmp = ?
      dest_size := dest_size * 2
      tmp := alloc_str(dest_size)
      FOR i = 1 TO str_data + dest_size / bytesperword DO
        tmp!i := dest!i
      dest := tmp
    }
    (dest + str_data)%ptr := c
    ptr := ptr + 1
  } REPEAT
  str_setlen(dest, ptr - 1)
  RESULTIS dest
}
