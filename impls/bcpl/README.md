* Mal in BCPL

This implementation expects to be built using the Cintcode BCPL compiler.

It uses several features that are not mentioned, or are
described as extensions by _BCPL - the language and its compiler_,
including:
 * The infixed byte operator `%`
 * Using `{` and `}` as section brackets
 * Using lower case and `_` in variable names

BCPL does support separate compilation, but running
separately-compiled modules in Cintsys seems to be ill-supported,
requiring use of the run-time dynamic loading mechanism, `loadseg`.
The examples in the manual all use `GET` to textually include library
source code in the main program, so mal does the same.
