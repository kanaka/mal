# SML-MAL

This is Make-A-Lisp in Standard ML.

## Building

Just run `make`.

Building requires a Standard ML compiler with basis library. This MAL
implementation has been tested and works with Poly/ML, MLton, and Moscow ML.

On Ubuntu, you can run `apt-get install polyml libpolyml-dev`.

By setting `sml_MODE` to `polyml`, `mosml`, or `mlton` on invoking `make` you
can select which compiler to use. The Makefile has some hacks to figure out
how to make the different compilers build everything.

## Running

You can build a `mal` binary from the final step with `make dist`:

```
$ make dist
$ ./mal
Mal [sml]
user> (map (fn* (x) (println "Odelay!")) [1 2 3 4 5])
Odelay!
Odelay!
Odelay!
Odelay!
Odelay!
(nil nil nil nil nil)
user> 
```
