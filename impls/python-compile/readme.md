# A Compiler from MAL to Python

This ongoing project aims to implement a compiler for [MAL (Make
a Lisp)](https://github.com/kanaka/mal/tree/master) to Python.
The code resides in `./impls/python/compile/`. Please note that
the reader and some primary functions were adopted from the MAL
implementation in Python by Joel Martin, whose code still resides
in `./impls/python/`.

## Passed All Official Tests (2,3,4,5,6,7,8,9,A)

Quickly test them by running:

``` shell
for ((i=2; i<=10; i++)); do
    [ $i -eq 10 ] && make "test^python-compile^stepA" || make "test^python-compile^step${i}"
    [ $? -ne 0 ] && { echo "Error occurred. Breaking loop."; break; }
done
```

## TODO

+ **Clean Up Code and Submit Pull Request:** Review the codebase
  for any cleanup or optimization opportunities. Once ready,
  submit a pull request for review and integration.
