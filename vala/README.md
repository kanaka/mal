# Vala implementation

Notes on building:

* With the Debian or Ubuntu packages `valac` and `libreadline-dev`
  installed, and GNU make, you should be able to build using the
  provided Makefile.

* The build will not be warning-clean, because the shared modules like
  `types.vala` and `core.vala` are shared between all the `stepN` main
  programs, and not all the steps use all the functions in the shared
  modules, and the Vala compiler has no way to turn off the warning
  about unused pieces of source code.

* The Vala compiler works by translating the program to C and then
  compiling that. The C compilation stage can sometimes encounter an
  error, in which case the compiler will leave `.c` source files in
  the working directory. If that happens, you can run `make clean-c`
  to get rid of them.

Design notes on the implementation:

* Vala has a reference counting system built in to the language.
  Garbage collection of mal objects is delegated to that system. So
  you can almost certainly contrive an un-GC-able cycle of objects by
  using atoms, and I haven't done anything about that.

* Vala has exceptions (which it calls 'error domains'), but they don't
  let you store an arbitrary data type: every exception subclass you
  make stores the same data, namely a string. So mal exceptions are
  implemented by storing a mal value in a static variable, and then
  throwing a particular Vala error whose semantics are 'check that
  variable when you catch me'.

* Vala's bare function pointers are hard to use, especially if you
  want one to survive the scope it was created in. So all the core
  functions are implemented as classes with a `call` method, which
  leads to a lot of boilerplate.

* To make `types.vala` work in step 2, when the `Env` type doesn't
  exist yet, I had to use `#if` to condition out the parts of the code
  that depend on that type.

* Mutability of objects at the Vala level is a bit informal. A lot of
  core functions construct a list by making an empty `Mal.List` and
  then mutating the `GLib.List` contained in it. But once they've
  finished and returned the `Mal.List` to their caller, that list is
  never mutated again, which means it's safe for the copying operation
  in `with-meta` to make a second `Mal.List` sharing the reference to
  the same `GLib.List`.
