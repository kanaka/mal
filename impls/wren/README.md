# Wren implementation

### Adding a time function

Since Wren doesn't have a time function, we add a `System.gettimeofday`
function which returns a float with the number of seconds since epoch (with
fractions of seconds).

This is done by applying the patch in `wren-add-gettimeofday.path` to Wren's
source code before compiling it (see `Dockerfile`).

### Wren interop

See examples in `tests/stepA_mal.mal` for usage of `wren-eval` to evaluate Wren
expressions inside a Mal program.
