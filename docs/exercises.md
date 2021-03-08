# Exercises to learn MAL

The process introduces LISP by describing the internals of selected
low-level constructs. As a complementary and more traditional
approach, you may want to solve the following exercises in the MAL
language itself, using any of the existing implementations.

You are encouraged to use the shortcuts defined in the step files
(`not`...) and `the `lib/` subdirectory (`reduce`...) whenever you
find that they increase the readability.

The difficulty is progressive in each section, but they focus on
related topics and it is recommended to start them in parallel.

Some solutions are given in the `examples` directory. Feel free to
submit new solutions, or new exercises.

## Replace parts of the process with native constructs

Once you have a working implementation, you may want to implement
parts of the process inside the MAL language itself. This has no other
purpose than learning the MAL language. Once it exists, a built-in
implementation will always be more efficient than a native
implementation. Also, the functions described in MAL process are
selected for educative purposes, so portability accross
implementations does not matter much.

You may easily check your answers by passing them directly to the
interpreter. They will hide the built-in functions carrying the same
names, and the usual tests will check them.
```
make REGRESS=1 TEST_OPTS='--hard --pre-eval=\(load-file\ \"../answer.mal\"\)' test^IMPL^stepA
```

- Implement `nil?`, `true?`, `false?`, `empty?` and `sequential` with
  another built-in function.

- Implement `>`, `<=` and `>=` with `<`.

- Implement `list`, `vec`, `prn`, `hash-map` and `swap!` as non-recursive
  functions.

- Implement `count`, `nth`, `map`, `concat` and `conj` with the empty
  constructor `()`, `empty?`, `cons`, `first` and `rest`.

  You may use `or` to make the definition of `nth` a bit less ugly,
  but avoid `cond` because its definition refers to `nth`.

  Let `count` and `nth` benefit from tail call optimization.

  Try to replace explicit recursions with calls to `reduce` and `foldr`.

  Once you have tested your solution, you should comment at least
  `nth`.  Many implementations, for example `foldr` in `core.mal`,
  rely on an efficient `nth` built-in function.

- Implement the `do` special as a non-recursive function. The special
  form will hide your implementation, so in order to test it, you will
  need to give it another name and adapt the test accordingly.

- Implement quoting with macros.
  The same remark applies.

- Implement most of `let*` as a macro that uses `fn*` and recursion.
  The same remark applies.
  A macro is necessary because a function would attempt to evaluate
  the first argument.

  Once your answer passes most tests and you understand which part is
  tricky, you should search for black magic recipes on the web. Few of
  us mortals are known to have invented a full solution on their own.

- Implement `apply`.

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

- Implement macros within MAL.

## More folds

- Compute the sum of a sequence of numbers.
- Compute the product of a sequence of numbers.

- Compute the logical conjunction ("and") and disjunction ("or") of a
  sequence of MAL values interpreted as boolean values.  For example,
  `(conjunction [true 1 0 "" "a" nil true {}])`
  should evaluate to `false` or `nil` because of the `nil` element.

  Why are folds not the best solution here, in terms of average
  performances?

- Does "-2-3-4" translate to `(reduce - 0 [2 3 4])`?

- Suggest better solutions for
  `(reduce str "" xs)` and
  `(reduce concat [] xs)`.

- What does `(reduce (fn* [acc _] acc) xs)` nil answer?

- The answer is `(fn* [xs] (reduce (fn* [_ x] x) nil xs))`.
  What was the question?

- What is the intent of
 `(reduce (fn* [acc x] (if (< acc x) x acc)) 0 xs)`?

  Why is it the wrong answer?

- Though `(sum (map count xs))` or `(count (apply concat xs))` can be
  considered more readable, implement the same effect with a single loop.
- Compute the maximal length in a list of lists.

- How would you name
  `(fn* [& fs] (foldr (fn* [f acc] (fn* [x] (f (acc x)))) identity fs))`?
