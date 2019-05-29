GET "libhdr"
GET "malhdr"

LET init_types() BE
  nil := TABLE 0

LET alloc_str(len) = VALOF
{ LET result = getvec(str_data + 1 + len / bytesperword)
  !result := 0
  type OF result := t_str
  result!str_len := 0
  (result+str_data)%0 := 0
  RESULTIS result
}

LET str_bcpl2mal(bcplstr) = VALOF
{ LET result = alloc_str(bcplstr%0)
  result!str_len := bcplstr%0
  FOR i = 0 TO bcplstr%0 / bytesperword DO
    result!(str_data + i) := bcplstr!i
  RESULTIS result
}
