Once you have a working implementation, you may want to implement
parts of the process inside the MAL language itself. This has no other
purpose than learning the MAL language. Once it exists, a built-in
implementation will always be more efficient than a native
implementation. Also, the functions described in MAL process are
selected for educative purposes, so portability accross
implementations does not matter much.

You may easily check your answers by passing them directly to the
interpreter. They will hide the built-in functions carrying the same
names, and the usual tests (with REGRESS=1) will check them. The
`runtest.py` script provide a convenient command-line parameter to
pass a command like 'load-file' before running the testsuite.

Some solutions are given in the `examples` directory. Feel free to
submit new solutions, or new exercises.


- Implement the following functions with other built-in functions.
  - `nil?`, `true?` and `false?`
  - `empty?`
  - `sequential?`

- Implement `>`, `<=` and `>=` with `<`.

- Implement the following non-recursive functions.
  - `hash-map`
  - `list`
  - `prn`
  - `swap!`

- Implement `map` with a recursion.

- Implement the `do` special as a non-recursive function. The special
  form will hide your implementation, so in order to test it, you will
  need to give it another name and adapt the test accordingly.

- Implement `let*` as a macro that uses `fn*` and recursion.
  The same remark applies.

- Implement `apply` as a macro.

- Implement maps using lists.
  - Recall how maps must be evaluated.
  - In the tests, you may want to replace `{...}` with `(hash-map ...)`.
  - An easy solution relies on lists alterning keys and values, so
    that the `hash-map` is only a list in reverse order so that the
    last definition takes precedence during searches.
  - As a more performant solution will use lists to construct trees,
    and ideally keep them balanced. You will find examples in most
    teaching material about functional languages.
  - Recall that `dissoc` is an optional feature. One you can implement
    dissoc is by assoc'ing a replacement value that is a magic delete
    keyword (e.g.: `__..DELETED..__`) which allows you to shadow
    values in the lower levels of the structure. The hash map
    functions have to detect that and do the right thing. e.g. `(keys
    ...)` might have to keep track of deleted values as it is scanning
    the tree and not add those keys when it finds them further down
    the tree.

- Implement quoting within MAL.

- Implement macros within MAL.
