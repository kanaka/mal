function PrintLn(str)
  let lines = split(a:str, "\n", 1)
  call writefile(lines, "/dev/stdout", "a")
endfunction

function s:buildlibvimreadline()
  if !filereadable("libvimextras.so")
    call system("make libvimextras.so")
  endif
endfunction

" Returns [is_eof, line_string]
function Readline(prompt)
  " Use the vimreadline() function defined in vimreadline.c and compiled
  " into libvimreadline.so
  call s:buildlibvimreadline()
  let res = libcall("libvimextras.so", "vimreadline", a:prompt)
  if res[0] == "E"
    return [1, ""]
  else
    return [0, res[1:]]
  endif
endfunction
