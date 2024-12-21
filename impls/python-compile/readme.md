# A Compiler from MAL to Python

[Make a Lisp (MAL)](https://github.com/kanaka/mal/tree/master) is a Lisp
programming language designed for educational purposes, aimed at teaching
people how to implement a programming language from scratch. It currently has
multiple implementations across various programming languages, including
[`python3`](https://github.com/kanaka/mal/tree/master/impls/python3), which
interprets MAL code but does not include a compiler.

This project is a fork of an earlier version of the official `python3`
implementation, with additional enhancements to include a compiler. As a
result, it achieves performance approximately 16 times faster than the
original interpreter. The source code for this implementation is located in
`./impls/python-compile/`.

## Passed All[^1] Official Tests 

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

[^1]: It current only passes test suites: 2, 3, 4, 5, 6, 7, 8, 9, A.
