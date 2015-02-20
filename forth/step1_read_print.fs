require reader.fs
require printer.fs

: read read-str ;
: eval ;
: print
    \ ." Type: " dup mal-type @ type-name safe-type cr
    pr-str ;

: rep ( str-addr str-len -- str-addr str-len )
    read
    eval
    print ;

create buff 128 allot
77777777777 constant stack-leak-detect

: read-lines
    begin
      ." user> "
      stack-leak-detect
      buff 128 stdin read-line throw
    while ( num-bytes-read )
      buff swap ( str-addr str-len )
      ['] rep
      \ execute safe-type
      catch ?dup 0= if safe-type else ." Caught error " . endif
      cr
      stack-leak-detect <> if ." --stack leak--" cr endif
    repeat ;

read-lines
cr
bye
