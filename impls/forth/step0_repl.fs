require types.fs

: read ;
: eval ;
: print ;

: rep
    read
    eval
    print ;

create buff 128 allot

: read-lines
    begin
      ." user> "
      buff 128 stdin read-line throw
    while ( num-bytes-read )
      dup 0 <> if
        buff swap
        rep type cr
      endif
    repeat ;

read-lines
