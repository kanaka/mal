require printer.fs

\ === basic testing util === /
: test=
  2dup = if
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


\ MalType tests

MalList new   MalList new   = 0 test=

MalList new dup MalList/car 5 swap ! MalList/car @   5 test=


\ Protocol tests

mal-nil
42 MalInt. mal-nil conj
10 MalInt. mal-nil conj conj
20 MalInt. swap conj
23 MalInt. mal-nil conj conj conj
pr-str s" (nil (20 (42) 10) 23)" str= -1 test=

\ map tests

s" one" MalString. s" one" MalString. mal= -1 test=
s" one" MalString. s" x" MalString. mal= 0 test=

MalMap/Empty
s" one" MalString. s" first" MalString. rot assoc
s" two" MalString. s" second" MalString. rot assoc
s" three" MalString. s" third" MalString. rot assoc

dup 99 s" two" MalString. rot get s" second" MalString. mal= -1 test=
dup 99 s" none" MalString. rot get 99 test=
drop

\ eval tests

require step2_eval.fs

mal-nil
    1 MalInt. swap conj
    2 MalInt. swap conj
    3 MalInt. swap conj
~~
mal-eval
~~

bye
