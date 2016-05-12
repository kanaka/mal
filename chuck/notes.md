# Step 1

- What if I don't have an OOP language?
- types.qx could be more promently mentioned...
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
