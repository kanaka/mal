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
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Error objects            y   y   y   y   ?   y   n   y   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Step #1                  y   y   y   y   ?   y   n   n   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 (srfi 1)                 y   y   y   y   y   y   y   n   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Step #4                  y   y   y   y   ?   y   n   n   ?   ?   ?
------------------------ --- --- --- --- --- --- --- --- --- --- ---
 Step #6                  y   y   y   y   ?   y   n   y   ?   ?   ?
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

Error objects
-------------

While there is an exception system, there is no need to use ``raise``
when the more convenient ``error`` is available.  Cyclone doesn't yet
support its helper procedures though, so I've written my own
replacements for them for its current internal representation (a list
of the message and the arguments).  This will most certainly break
once it actually starts supporting them...

Step #1
-------

This is about whether the tests for step #1 have been passed
successfully.  Foment fails here as it sends ANSI escapes to the test
rig, but works again after a recent bugfix.  Cyclone had a
show-stopping bug where the last symbol token had one garbage byte too
many, I've fixed this and another bug about the write representation
locally for now.

(srfi 1)
--------

The infamous list processing SRFI.  It contains many goodies you'd
taken for granted in other programming languages, such as a procedure
for retrieving the last element of a list.  All implementation except
Foment have it, so I just write my own list helpers as needed.  No big
deal.

Step #4
-------

Step #2 and #3 worked without any hitch, step #4 however exposes some
shortcuts I've taken.  R7RS states for certain procedures that
evaluation order is unspecified to allow for optimizations for pure
functions, Cyclone makes use of this for ``map``.  ``begin`` is
guaranteed to go from left to right, an explicit loop also works.  My
clever trick of repurposing ``read`` and ``write`` for parsing and
serializing machine-readable strings backfired as R7RS only specifies
that backslashes and double-quotes need to be quoted, newlines may be
quoted but don't have to.  For this reason I rolled my own serializer
that takes care of all of those characters.

Step #6
-------

Step #5 wasn't a problem either, however this step introduced basic
file I/O.  To read a complete file into a string I read a fixed size
string from the file port until EOF is returned and stuff each chunk
into the string port.  This strategy yields an infinite loop with
Cyclone as it doesn't ever return EOF.  I've handed in a PR to fix
this.

Bug reports
===========

- https://github.com/justinethier/cyclone/issues/216
- https://github.com/justinethier/cyclone/issues/217
- https://github.com/justinethier/cyclone/issues/219
- https://github.com/justinethier/cyclone/issues/220
- https://github.com/justinethier/cyclone/issues/221
- https://github.com/justinethier/cyclone/pull/222
- https://github.com/justinethier/cyclone/issues/224
- https://github.com/justinethier/cyclone/issues/225
- https://github.com/leftmike/foment/issues/14
- https://github.com/leftmike/foment/issues/15
- https://github.com/leftmike/foment/issues/16
- https://github.com/leftmike/foment/issues/17
- https://github.com/leftmike/foment/issues/18
