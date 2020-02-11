require printer.fs

\ === basic testing util === /
: test=
  2dup m= if
    2drop
  else
    cr ." assert failed on line " sourceline# .
    swap cr ." | got " . cr ." | expected " . cr
  endif ;

\ array function tests
create za 2 , 6 , 7 , 10 , 15 , 80 , 81 ,

7 za 2 array-find   -1 test= 0 test=
7 za 6 array-find   -1 test= 1 test=
7 za 10 array-find  -1 test= 3 test=
7 za 81 array-find  -1 test= 6 test=
7 za 12 array-find   0 test= 4 test=
7 za 8  array-find   0 test= 3 test=
7 za 100 array-find  0 test= 7 test=
7 za 1 array-find    0 test= 0 test=
6 za 81 array-find   0 test= 6 test=

10 new-array
1 swap 0  5 array-insert
2 swap 1  7 array-insert
3 swap 3 12 array-insert
4 swap 4 15 array-insert
5 swap 5 20 array-insert

dup 0 cells + @   5 test=
dup 1 cells + @   7 test=
dup 2 cells + @  10 test=
dup 3 cells + @  12 test=
dup 4 cells + @  15 test=
dup 5 cells + @  20 test=


\ Protocol tests

: t1
mal-nil
42 MalInt. mal-nil conj
10 MalInt. mal-nil conj conj
20 MalInt. swap conj
23 MalInt. mal-nil conj conj conj
pr-str s" (nil (20 (42) 10) 23)" str= -1 test=

1500 MalInt. 1500 MalInt. test=

\ MalList tests

here 1 MalInt. , 2 MalInt. , 3 MalInt. , here>MalList
4 MalInt. swap conj
5 MalInt. swap conj
pr-str s" (5 4 1 2 3)" str= -1 test=

\ map tests

s" one" MalString. s" one" MalString. test=
s" one" MalString. s" x" MalString. m= 0 test=

MalMap/Empty
1000 MalInt. 1100 rot assoc
2000 MalInt. 2100 rot assoc
3000 MalInt. 3100 rot assoc

dup 99 2000 MalInt. rot get 2100 test=
dup 99 4000 MalInt. rot get 99 test=
drop

MalMap/Empty
s" one" MalString. s" first" MalString. rot assoc
s" two" MalString. s" second" MalString. rot assoc
s" three" MalString. s" third" MalString. rot assoc

dup 99 s" two" MalString. rot get s" second" MalString. test=
dup 99 s" none" MalString. rot get 99 test=
drop

99 MalInt. 10 MalInt. MalMap/Empty get 99 MalInt. test=

;
t1

\ eval tests

require step2_eval.fs

: t2
mal-nil
    1 MalInt. swap conj
    2 MalInt. swap conj
    3 MalInt. swap conj
mal-eval
;
t2

bye
