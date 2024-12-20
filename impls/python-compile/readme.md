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
[./mal]$ \
for ((i=2; i<=10; i++)); do
    [ $i -eq 10 ] && make "test^python-compile^stepA" || make "test^python-compile^step${i}"
    [ $? -ne 0 ] && { echo "Error occurred. Breaking loop."; break; }
done
```

## ~16 Times Faster Than The Interpreted Version

Per the official performance test suite, `python-compile` is near
16 times faster than the interpreted version.

``` shell
[./mal]$ make "perf^python"
----------------------------------------------
Performance test for python:
Running: env STEP=stepA_mal MAL_IMPL=js python_MODE=python ../python/run ../tests/perf1.mal
Elapsed time: 1 msecs
Running: env STEP=stepA_mal MAL_IMPL=js python_MODE=python ../python/run ../tests/perf2.mal
Elapsed time: 4 msecs
Running: env STEP=stepA_mal MAL_IMPL=js python_MODE=python ../python/run ../tests/perf3.mal
iters over 10 seconds: 9311

[./mal]$ make "perf^python-compile"
----------------------------------------------
Performance test for python-compile:
Running: env STEP=stepA_mal MAL_IMPL=js ../python-compile/run ../tests/perf1.mal
Running: env STEP=stepA_mal MAL_IMPL=js ../python-compile/run ../tests/perf2.mal
Running: env STEP=stepA_mal MAL_IMPL=js ../python-compile/run ../tests/perf3.mal
iters over 10 seconds: 148519
```

## TODO

+ Document how the design of the compiler.

+ Finish implementing `compile_file`.

+ Wait for pull request decision: https://github.com/kanaka/mal/pull/653
