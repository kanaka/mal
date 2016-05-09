# Mal/Make-a-Lisp Implmentation Hints

<a name="milliseconds"></a>

### How do I get milliseconds since epoch for the "time-ms" function?
### Does the "time-ms" function have to return millisecond since epoch?

Most languages usually have some way to do this natively even though
it might be buried deeply in the language. If you are having trouble
finding how to do this in your target language, consider asking the
question on stackoverflow (if it has not been asked already) or asking
on a discussion channel for your language because there is a good
chance somebody there knows how and will answer quickly (if there is
a native way at all).

As a last resort you can always shell out and call the date command
like this:

```
date +%s%3N
```

There are currently two implementations where this method was
necessary (probably): bash and make. Unfortunately this method is
limited to Linux/UNIX.

Also, "time-ms" technically just needs to return accurate milliseconds
since some arbitrary point in time (even program start) in order to be
used correctly for timing/benchmarking. For consistency it is best if
it returns epoch milliseconds, but this is not strictly required if
you language limitations make it difficult (e.g. size limit of
integers).


<a name="function_references"></a>

### How do I implement core/native functions if my language does not have any sort of function references (function pointers, closures, lambdas, etc)?
### How do I implement mal functions in step4 if I do not have function references?

There are very few language that do not have any sort of function
references so I suggest asking about the specific problem you are
having on stackoverflow or a discussion channel for your language. In
the rare case where you have a language without some sort of function
reference abstraction, then you may have to implement a single
function with a large switch statement (or equivalent) that calls out
to the appropriate native core function ("+", "list", "throw", etc).
In other words, you create a function that implements "function
references" rather than using a feature of your language.  You will
still need to store the symbol names for those function in the base
REPL environment but you will have some sort of tagging or marker that
will indicate to the `EVAL` function that it should call your "big
switch" function.

In addition, if your language has no sort of closure/anonymous
function capability (note that with sufficient object oriented
features you can implement closure like functionality), then in step4
you will need to borrow the way that functions are implemented from
step5. In other words, functions become a normal data type that stores
the function body (AST), the parameter list and the environment at the
time the function is defined. When the function is invoked, `EVAL`
will then evaluate these stored items rather than invoking a function
closure. It is less convenient to have to do this at step4, but the
bright side is that step5 will be simpler because you just have to
implement the TCO loop because you have already refactored how
functions are stored in step4.

<a name="IO"></a>

### How do I implement terminal input and output in a language which does not have standard I/O capabilities?

If your target language has some way to get data in and out while it
is running (even if it is not standard terminal or file I/O) then you
will need to create some sort of wrapper script (see
`vimscript/run_vimscript.sh`) or call out to a shell script (see
`make/readline.mk` and `make/util.mk`) or implement some other
"appropriate" hack to to get the data in and out. As long
as your implementation can be used with the test runner and the hack
is just for working around I/O limitations in your target language,
it is considered legitimate for upstream inclusion.

### How do I read the command-line arguments if my language runtime doesn't support access to them?

Most languages give access to the command-line arguments that were passed to
the program, either as an arguement to the `main` function (like `argc` and
`argv` in C) or as a global variable (like `sys.argv` in Python).  If your
target language doesn't have such mechanisms, consider adding a wrapper script
that will read the command-line arguments that were passed to the script and
pass them to the program in a way that the program can read.  This might be
through an environment variable (if the target language allows reading from
environment variables) or through a temporary file.


<a name="no_reader_object">

### How can I implement the reader without using a mutable object?

You do not need a mutable object, but you do need someway of keeping
track of the current position in the token list. One way to implement
this is to pass both the token list and the current position to the
reader functions (read_form, read_list, read_atom, etc) and return
both the parsed AST and the new token list position. If your language
does not allow multiple values to be returned from functions then you
may need to define a data structure to return both the new position
and the parsed AST together. In other words, the pseudo-code would
look something like this:

```
ast, position = read_list(tokens, position)
```

---

Answers for the following questions are TBD.

### How do I implement slurp in a language without the ability to read raw file data?

<a name="exceptions">

### How do I support raising/throwing arbitrary objects in a language that does not support that?
### What do I do if my implementation language only supports string exceptions?



