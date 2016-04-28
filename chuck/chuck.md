- I've found a potential bug in their substring function:
  https://github.com/ccrma/chuck/issues/55
- the manual isn't up to date, so you need to look at `VERSIONS` and
  the examples instead, sometimes the sources, too
- the manual only speaks of the debug syntax for printing
  (`<<<foo>>>;` which goes to stderr), I've found a `chout` object you
  can send strings to for outputting to stdout
- quitting is done via `C-c` only
- you'll want to use `--silent` to disable audio errors/processing,
  but then the process will use 100% CPU and ignore any waiting
- stdin handling is terrible:
  - the manual shows a keyboard example with HID devices, but it
    doesn't work on linux
  - there's a "hacked" `ConsoleInput` class with only an example file
    for it, it works for most of the part, but doesn't accept `C-d`
  - the obvious alternative is printing a prompt manually, then
    waiting for `KBHit` events and printing them, but that's rather
    tedious as you'd have to convert the ascii numbers into chars
    yourself and make a buffer-like thing
  - I've also considered writing a thing sending OSC events per
    keyboard hit and processing these in ChucK as they come in, but
    that would most likely not work with the test harness ._.
- the OOP system is seriously weird
  - influenced by C++ *and* java
  - one public class per file
  - to export functionality, you must use a public class (and static
    functions/variables)
  - if you use static variables, you can't assign values to them
    directly, you'll have to do that after the class has been defined
  - no interfaces
  - no generics (copy/paste code for all types you need!)
  - no unions (use Object, then cast to the correct type)
  - there is no obvious way of casting to arrays of types
  - no private (things are public by default, public keyword is used
    to export code)
  - no self-references in classes (so no trees, static "constructors"
    work though)
  - no meaningful way of working with null for primitive types (mutate
    a reference and look at the return code instead)
  - no boxed versions of primitive types
  - no automatic boxing/unboxing
- No module system
  - `Machine.add(file)` is the only mechanism available from code (no
    read all file contents and eval), but if you use it, it defers
    loading the files until the file it's used in, rendering it
    useless
  - Therefore the only way to make use of it is writing a file that
    only consists of these instructions
  - The only practical alternative is specifying all files you need
    loaded in the right order when starting chuck
  - That's why I wrote a runner script extracting `// @import file.ck`
    lines (hello JS!) and running chuck with them
- No real exception system
  - The VM is able to throw exceptions (out of bounds, nullpointer),
    but you can't do anything about them and only get a hint what kind
    of operation caused it (no stacktrace or anything)
  - No user-definable exceptions, no mechanism to catch or throw them
    (other than intentionally doing something illegal)
  - This means that you must use C-style error checking by converting
    the potentially erroneous functions into returning a status code
    and mutating a reference passed to them as argument which is
    highly weird in a otherwise Java-like language
- Other oddities
  - strict distinction between assigning values and references with
    two separate operators for them (`<<` for array append doesn't
    seem to care though)
  - strings are supposedly reference types, yet you can assign them
    with the regular operator...
  - `<<` on an `type[]` gives you a weird error as you need to use an
    `type[0]` (and a `type[]` is merely a reference...)
