# A Compiler from MAL to Python

This ongoing project aims to implement a compiler for [MAL (Make
a Lisp)](https://github.com/kanaka/mal/tree/master) to Python.
The code resides in `./impls/python/compile/`. Please note that
the reader and some primary functions were adopted from the MAL
implementation in Python by Joel Martin, whose code still resides
in `./impls/python/`.

## TODO

+ **Implement Quasiquote, Unquote, and Splice-unquote:** Develop
  functionality for quasiquote, unquote, and splice-unquote.

+ **Implement Macros (Section 8):** Proceed with implementing
  macros only after careful consideration and potential
  reimplementation of `compile_fn`.

+ **Implement Try and Catch (Section 9):** Add support for try
  and catch constructs.

+ **Test MAL Against Official Test Cases:** Verify the
  functionality of this MAL implementation against the official
  test cases.

+ **Clean Up Code and Submit Pull Request:** Review the codebase
  for any cleanup or optimization opportunities. Once ready,
  submit a pull request for review and integration.
