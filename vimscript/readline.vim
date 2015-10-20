function PrintLn(str)
  let lines = split(a:str, "\n", 1)
  call writefile(lines, "/dev/stdout", "a")
endfunction

function s:buildlibvimreadline()
  if !filereadable("libvimreadline.so")
    call system("make libvimreadline.so")
  endif
endfunction

" Returns [is_eof, line_string]
function Readline(prompt)
  " Use the vimreadline() function defined in vimreadline.c and compiled
  " into libvimreadline.so
  call s:buildlibvimreadline()
  let res = libcall("libvimreadline.so", "vimreadline", a:prompt)
  if res[0] == "E"
    return [1, ""]
  else
    return [0, res[1:]]
  endif
endfunction
