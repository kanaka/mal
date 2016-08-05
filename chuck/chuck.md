- I've found a potential bug in their substring function:
  https://github.com/ccrma/chuck/issues/55
- later I've found one in their regex replace function, too:
  https://github.com/ccrma/chuck/issues/60
- this suggests there hasn't been much testing done on things
  unrelated to audio which is not that unexpected in an audio
  programming language, but still...
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
  - worse, you can't even declare anything that's not a primitive, so
    if you want to declare a reference type, use the reference
    operator instead...
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
  - no upcasting/downcasting
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
  - This means that you should use C-style error checking by
    converting the potentially erroneous functions into returning a
    status code and mutating a reference passed to them as argument
    which is highly weird in a otherwise Java-like language
  - An alternative is defining an error object (which belongs to the
    same supertype as the other legal return values) and checking its
    type by inspecting the user-tracked type field
- No function pointers/functors/closures
  - This is a bit unexpected as if you leave away the parentheses
    holding the argument list and debug print a function, you'll see
    it being recognized as a function, yet you can't store it anywhere
    for passing it around
  - This is not quite right as you can store it in an `Object`, just
    not call it in any way or cast it to a function type
  - So you get to implement functors and closures yourself...
  - A functor is a class with a call method taking an argument list
    and executing the code of the function you intend to pass around
  - To use it, store an instance of its class somewhere, then use its
    call method with an argument list
  - Closures can be implemented with a data structure holding a
    snapshot of the current environment, the parameter list and AST,
    the last two being a way of representing an anonymous function.
- Other oddities
  - strict distinction between assigning values and references with
    two separate operators for them (`<<` for array append doesn't
    seem to care though)
  - strings are supposedly reference types, yet you can assign them
    with the regular operator...
  - `<<` on an `type[]` gives you a weird error as you need to use an
    `type[0]` (and a `type[]` is merely a reference...)
  - The compiler will find lots of mistakes for you, but cannot figure
    out code branches not returning anything which means that return
    type violations will blow up in your face unless there's a
    reasonable default value (null for `Object` isn't, 0 for `int` and
    "" for `string` is)
  - If you abuse the type system too much, chances are you get a
    segfault or assert instead of an exception...
  - Debug print shows the object and its type if you pass one
    argument, if you pass more than one, it prints the concatenation
    of their representations instead, so it's a bit hard to make out
    what is a debug print and what isn't
  - there are no hash maps, just the possibility to use a string key
    on an array for storing and fetching contents (like in PHP, eww)
    and no way of retrieving keys/values or even iterating over these
  - I think I've spotted a weird scoping bug that prefers a member
    variable over a local variable after nesting scopes, therefore I
    consider the language to not implement proper lexical scoping
  - another proof of it is declaring variables in consequent if-blocks
    as that gives you an error instead of being permitted as they
    should be in different local scopes...
