# Step 1

- What if I don't have an OOP language?
- types.qx could be more prominently mentioned...
- A table with all types and suggested object names would be hugely
  useful
- Same for a list of all errors and their messages
- Mention return types and argument types consistently
- More on int/float and their grammar (int is mentioned implicitly in
  the ASCII art, nothing on signs or bases or their lack of)
- Note that a string must be parsed for the `print_readably` thing to
  work and mention how one could do that (like, by using a `read` or
  `eval`-like thing or alternatively, chopping off the surrounding
  quotes and doing the inverse transformation of the printing)
- How is an atom printed?

# Step 2

- What if my language doesn't support lambdas, let alone passing
  around named functions? Ideally write something about
  implementing/using functors/delegates or replacing that namespace
  with a big switch as with VHDL.  Another problem is that if you
  choose a different solution in step 4, step 2 could end up no longer
  functional...
- What kind of error (read: what message?) is raised when no value can
  be looked up for the symbol?  Is it arbitrary?  Do I need to extend
  my error handling to allow for format strings?
- It would be worth a mention that you should extend the printer to
  handle "native" functions (or in oldtimey terms, subrs)

# Step 3

- You should modify both eval_ast *and* EVAL
- Suggest the trick with destructuring the AST into `a0`, `a1`,
  etc. variables for easier access.  Perhaps this can be used to clear
  up the general language used with AST manipulation (like, first
  parameter and second list element)?
- What does def! return?  Emacs Lisp for instance returns the symbol
  whereas the tests suggest the value should be returned instead...

# Step 4

- "Implement the strings functions"
- The "no closures" paragraph isn't quite clear.  Asides from that, do
  native functions don't really need to be wrapped the same way as the
  `fn*` objects, just introduce another type (like, a Subr and a Func
  type) and do a check before applying the arguments to it
- Why does the guide say that the first argument of `count` can be
  treated as list, yet there's a test performing `(count nil)` and
  expecting zero as result?
- Does it make sense to compare, say, atoms in `=`?

# Step 5

- "This is especially important in Lisp languages because they tend to
  prefer using recursion instead of iteration for control structures."
  <- I'd argue it's less of a lisp thing (see everything else related
  to CL) and more a thing functional programming proponents have
  considered more elegant than introducing iteration constructs (see
  haskell, ocaml, erlang)
- It's not really clear that the TCO change for `let*` involves the
  form you'd normally pass to `EVAL` to become the new `ast`.  I had to
  reread this a few more times to understand that the "second `ast`"
  is actually its third argument...
- Where did the check for `do` not being broken by TCO go?
- What's the deal with the `quux/tests/step5_tco.qx` file?

# Step 6

- "The closure calls the your EVAL function […]."
- I still don't have any closures.  How the heck do I implement
  `eval`?  What about `swap!`?
- It would be useful to mention that `swap!` sort of requires
  implementing `apply` first...

# Step 7

- Why the scare quotes for splicing?
- "Before implementing the quoting forms, you will need to implement
  some supporting functions in the core namespace:" should be one list
  item
- "this function takes a list as its second parameter and returns a
  new list that has the first argument prepended to it." reads backwards
- The quasiquote paragraph is hard to read
- It's rather confusing to refer to the argument of `ast` and to an
  `ast` parameter, perhaps name the latter a form?
- What could also help would be a visualization of the four
  conditionals:
  - \`42, \`()
  - \`~foo
  - \`(~@foo) and more
  - \`(42 ~@foo) and everything else
- Mal/mal is inconsistently capitalized
- "Expand the conditional with reader `read_form` function to add the
  following four cases" is again weird, better refer to the
  `read_form` function in reader.qx
- "concat should support concatenation of lists, vectors, or a mix or
  both." <- "or a mix or both" is redundant

# Step 8

- "In the previous step, quoting enabled some simple manipulation [of]
  data structures"
- The macroexpand function step refers to call/apply, it's unclear how
  to proceed if you don't have such a thing
- How should the exception for invalid `nth` access look like?  Also,
  why is it an exception and not an error like with the reader?
- How can `first` take a list (or vector), but work on `nil`?
- The description of `rest` is inconsistent with the tests
- "In the main program, use the rep function to define two new control
  structures macros."
- Why does the definition of `cond` use `throw` although it's only
  introduced in the next chapter?

# Step 9

- It's not really clear that you really just have a `try*` special
  form, with `catch*` merely existing inside it...
- Another thing to clarify is that the exception value is a string
  containing the message you'd see (unless you're using `throw`)
- Generally, it would be better to explain the general exception
  handling mechanism (with some examples), then showing how one
  implements it for both languages with and without exceptions
- Another way than using a global variable is introducing an error
  type next to the other MAL types and checking whether something a
  function returned is one, although the hint about returning one at
  each use of `EVAL` still stands...
- Shouldn't either trick be mentioned at the beginning, simply because
  you'll need it in a language without exceptions to do error handling?
- Why this bizarre treatment for `keyword`?  Why is there no test for
  it?
- Is there a test for whether hash maps deduplicate identical keys
  when using `hash-map` or `assoc`?
- What exactly are keys the specification for `dissoc`, `get` and
  `contains?` are speaking of?  Can I assume these are either strings
  or keywords?
- Why is it not documented that `get` may take `nil` instead of a map?
- Perhaps it's worth adding more tests involving symbols to ensure
  that functions using apply internally don't evaluate their args?

# Step A

- "Add meta-data support to mal functions." <- Shouldn't you mention
  that this involves implementing `with-meta` and `meta`?
- "TODO. Should be separate from the function macro flag." <- Why is
  this even related?
- It would be worth to mention that `with-meta` shall clone its
  argument to avoid one of the more sneaky test failure reasons
- "The value of this entry should be a mal string containing the name
  of the current implementation."
- "When the REPL starts up (as opposed to when it is called with a
  script and/or arguments), call the rep function with this string to
  print a startup header: `"(println (str \"Mal
  [\" *host-language* \"]\"))".`" <- proof that you better quote these
  because the asterisks just disappear...
