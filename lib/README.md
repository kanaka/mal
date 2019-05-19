This directory contains general-purpose reusable code that does not
fit in the process.

The split in small files is motivated by implementations too limited
to load a single big file, but MAL has no proper module management.

However, here are some guidelines.

- Begin with an one-line ;; short description

- End with `nil`, so that the result of `load-file` is conveniently
  short when loading manually and predictilbe for automatic testing

- Describe the restrictions on each parameter in comments.

- Define private symbols in hidden environments when possible. If this
  is not possible, for example for macros, give them a name starting
  with an underscore.

- Support successive imports safely by giving the same definitions
  again.

If a module provides tests, you may run against an implementation IMPL
with these commands.
```
make IMPL^stepA
cd tests
python ../runtest.py lib/MODULE.mal ../IMPL/run
```
