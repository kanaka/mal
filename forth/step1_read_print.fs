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
      dup 0 <> if
        buff swap ( str-addr str-len )
        ['] rep
        \ execute ['] nop \ uncomment to see stack traces
        catch ?dup 0= if
            safe-type cr
            stack-leak-detect <> if ." --stack leak--" cr endif
        else { errno }
            begin stack-leak-detect = until
            errno 1 <> if
                s" forth-errno" MalKeyword. errno MalInt. MalMap/Empty assoc
                to exception-object
            endif
            ." Uncaught exception: "
            exception-object pr-str safe-type cr
        endif
      endif
    repeat ;

read-lines
cr
bye
