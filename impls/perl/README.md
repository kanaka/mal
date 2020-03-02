# Notes on the mal implementation in Perl5.

This implementation should work in any perl from 5.19.3 onwards.
Earlier versions are likely to work too as long as you install a new
List::Util.  The implementation uses the experimental `switch`
feature, which may make it vulnerable to future changes in perl.

Mal objects are all in subclasses of `Mal::Type`, and can be treated
as scalar, array, or hash references as appropriate.

Metadata support uses `Hash::Util::FieldHash` to attach external
metadata to objects.  This means that in the metadata system imposes
no overhead on the normal use of objects.

Hash-maps are slightly magical.  They're keyed by the stringified
versions of mal objects, and `Mal::Scalar` overloads stringification
so that this works properly.

Tail-call optimisation uses Perl's built-in `goto &NAME` syntax for
explicit tail calls.  This allows functions defined by `fn*` to be
implemented as functions at the Perl layer.

Perl's garbage-collection is based on reference counting.  This means
that reference loops will cause memory leaks, and in particular using
`def!` to define a function will cause that function to have a
reference to the environment it's defined in, making a small reference
loop and hence a memory leak.  This can be avoided by carefully
undefining any function before it goes out of scope.
