Key
===

- Chibi: c
- Kawa: k
- CHICKEN: C
- Gauche: g
- Picrin: p
- Sagitarrius: s
- Cyclone: §
- Foment: f
- Guile: G
- Racket: r
- Larceny: l

- Works: y
- Doesn't: n
- Sort of: x
- Unknown: ?

Matrix
======

======================== === === === === === === === === === === ===
 Scheme implementations   c   k   C   g   p   s   §   f   G   r   l
======================== === === === === === === === === === === ===
 R7RS support             y   y   y   y   y   y   y   y   n   ?   x
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Console I/O              y   y   y   y   y   y   x   n   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Step #0                  y   y   y   y   y   y   y   n   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Modules                  y   y   y   y   n   y   y   y   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Automatic library load   y   y   x   y   n   y   x   y   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 (scheme char)            y   y   y   y   n   y   y   y   ?   ?   ?
======================== === === === === === === === === === === ===

Notes
=====

R7RS Support
------------

This is about whether I can write a script in R7RS Scheme and
successfully execute it with the implementation.  Guile didn't pass
this test and the manual merely mentions it implements a few
R7RS-features (which is far from sufficient), Racket supposedly has
inofficial support for it, Larceny refuses loading up anything else
than a R7RS library.

Console I/O
-----------

In step 0 a REPL is implemented that echoes back user input and quits
on EOF (signalled by ``C-d``).  Cyclone is weird here because its
``read-line`` procedure includes the trailing newline (fixed
upstream), Foment doesn't recognize EOF from console, but does so fine
with input redirection (as in ``(with-input-from-file "..."
read-line)``), a bug report for that is still pending.

Step #0
-------

This is about whether the tests for step #0 have been passed
successfully.  Foment fails this as it detects it's wired up to a tty
and probes for its cursor position before initializing its readline
implementation.  This makes the test rig fail detecting a prompt.  A
bug has been submitted upstream to rectify this.

Modules
-------

MAL requires a certain amount of modularization by splitting the
interpreter up into multiple files, for this R7RS libraries are a
natural fit.  This is purely about whether the implementation allows
using code from a library file inside a script file.  The only one not
passing this requirement is Picrin as it neither allows loading up
multiple files nor automatically loads up extra files.  This leaves me
with just ``load`` as primitive, but this is not sufficient because I
need a relative load facility and the details on how its argument is
treated are implementation-specific.

Automatic library load
----------------------

R7RS libraries are specified as a list of identifiers, commonly
translated to a nested path (for example ``(foo bar)`` translates to
``foo/bar.sld``) that is looked up in the include locations and loaded
automatically.  CHICKEN translates them to ``foo.bar.scm`` and doesn't
load up source files automatically, instead you'll have to compile
libraries to importable shared libraries.  Similarly, Cyclone only
loads up libraries after they've been compiled.  Picrin doesn't do
anything in this regard, so only something like concatenating source
files (hello JS!) might work out.

(scheme char)
-------------

R7RS is split up into many base libraries, including one for char
procedures.  This is necessary for tokenization of user input,
unfortunately Picrin doesn't implement this namespace at all and
throws parts of it into ``(scheme base)`` instead, without the
mandatory unicode support.  This and the preceding failures are reason
enough for me to exclude it from this comparison.
