require reader.fs
require printer.fs

: read read-str ;
: eval ;
: print pr-str ;

: rep
    read
    eval
    print ;

create buff 128 allot

: read-lines
    begin
      ." user> "
      buff 128 stdin read-line throw
    while
      buff swap
      rep safe-type cr
    repeat ;

\ s" 1   (42 1 (2 12 8)) 35" swap 1+ swap .s read-str .s
\ s" 7" .s read-str .s
\ cr
\ pr-str safe-type cr
\ new-str s" hello" str-append char ! str-append-char safe-type
\ s\" he\nllo" MalString. pr-str safe-type cr

read-lines
cr
bye
