GET "libhdr"
GET "malhdr"

LET readline(prompt, buf) BE
{ LET p = 1
  LET ch = 0
  writes(prompt)
  deplete(cos)
  { ch := rdch()
    buf%p := ch
    p := p + 1
  } REPEATUNTIL ch = '*n'
  buf%0 := p - 1
}
