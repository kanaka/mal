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

* Vala has a reference counting system built in to the language, but
  that's not enough to implement mal sensibly, because the common
  construction `(def! FUNC (fn* [ARGS] BODY))` causes a length-2 cycle
  of references: the environment captured in `FUNC`'s function object
  is the same one where `def!` inserts the definition of `FUNC`, so
  the function and environment both link to each other. And either
  element of the cycle could end up being the last one referred to
  from elsewhere, so you can't break the link by just making the right
  one of those references weak. So instead there's a small garbage
  collector in `gc.vala`, which works by being the only part of the
  program that keeps a non-weak reference to any `Mal.Val` or
  `Mal.Env`: it links all GCable objects together into a list, and
  when the collector runs, it unlinks dead objects from that list and
  allows Vala's normal reference counting to free them.
