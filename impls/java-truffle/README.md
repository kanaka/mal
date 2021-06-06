# Truffle Mal

This Mal is implemented in Java using the [Truffle Framework](https://github.com/oracle/graal/blob/master/truffle/README.md).
Truffle is a library for implementing interpreters. When
these interpreters are run on GraalVM, the GraalVM compiler
is able to JIT compile interpreted programs using a technique
called [partial evaluation](https://en.wikipedia.org/wiki/Partial_evaluation).

Partially evaluating an interpreter plus a program to produce compiled
code requires a careful balance. If every last bit of interpreter code
(including supporting libraries, etc.)
is subject to partial evaluation, the result will explode to
unreasonable size. Boundaries must be drawn. Exclude too much, though,
and the speed up resulting from compilation may not be worth the
effort of the compilation.

Truffle's "thesis" is that a small set of primitives are sufficient to make
JIT compilation via partial evaluation practical.
These primitives feed runtime data collected by the executing interpreter
to the compiler, allowing it to _specialize_, or optimistically
simplify, the interpreter code at compilation time. The compiler inserts
lightweight runtime checks of the assumptions that justify its
simplifications. If the checks fail, the compiled code is _de-optimized_,
and control is returned to the interpreter.
See [Practical Partial Evaluation for High-Performance Dynamic Language Runtimes](http://chrisseaton.com/rubytruffle/pldi17-truffle/pldi17-truffle.pdf), from PLDI 2017, for a deeper treatment of the ideas behind Truffle.

The Truffle Mal implementation is my attempt at putting the Truffle thesis
to the test.

Can I, an engineer without a background in compiler design, use Truffle to
implement an interpreter for a dynamic language (Mal) that substantially
outperforms the existing Java interpreter for Mal?

*The Short Answer: Yup.*

```bash
   # Recursive Fibonacci on OpenJDK 11 with java mal
   $ ./run ../tests/fib.mal 30 10
   Times (in ms) for (fib 30) on java: [2062 1809 1814 1777 1772 1791 1725 1723 1786 1745]
   
   # Recursive Fibonacci on GraalVM with java-truffle mal
   $ ./run ../tests/fib.mal 30 10
   Times (in ms) for (fib 30) on java-truffle: [280 142 21 26 22 75 21 26 21 24]
   
   # That's an 82x speed-up! Just out of curiosity...
   # How does Clojure on OpenJDK 11? We'll even throw in a type hint.
   $ lein repl
   Clojure 1.10.0
   OpenJDK 64-Bit Server VM 11.0.7+10-post-Ubuntu-2ubuntu218.04
   user=> (defn fib [^long n] (if (= n 0) 1 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))
   #'user/fib
   user=> (dotimes [i 5] (time (fib 30)))
   "Elapsed time: 32.0791 msecs"
   "Elapsed time: 31.7552 msecs"
   "Elapsed time: 31.5361 msecs"
   "Elapsed time: 31.4796 msecs"
   "Elapsed time: 31.4541 msecs"
```

A recursive Fibonacci computation is _obviously_ not sufficient to characterize the
performance of our implementation (and as we'll see, it turns out to be
something of a best-case scenario), but it sure looks impressive!

Do more complicated Mal programs show similar speed-ups?

How much simplicity did we have to sacrifice in the name of performance?

Was it worth it?

How much of the speed-up is really attributable to the Truffle/GraalVM combo,
and how much came from putting more time into the code itself?

We'll explore the answers to these questions together in the remainder!

## Disclaimers

*First and foremost*: To the extend that this experiment _succeeds_ in its goal of
producing an efficient Mal implementation, the credit is due to the teams
behind Truffle and GraalVM. To the extend that this experiment _fails_, the blame
falls on *me*! The reader should assume, by default, that any deficiencies in
this Mal implementation are due to my own failure to understand or
properly apply the tools at my disposal, and _not_ due to any fundamental
limitations of Truffle or GraalVM.

*Second:* This Mal implementation is _not_ idiomatic Java, and it's _not_ an
idiomatic application of Truffle. The project's
unusual organization (large numbers of package-private classes bundled
into single files like Types.java, substantial duplication between step files)
represent my attempt to adhere both to the spirit of Mal's
pedagogical approach and the organization of the existing Java implementation.
Consequently I have abused Truffle in several ways (that I am aware of, and perhaps
others that I am not?). Each Mal step
registers a distinct Truffle implementation whose language id has the form "mal_step${n}".
The languages for each step have distinct AST node sub-classes, but they share
the built-in AST nodes in Core.java and the runtime types in Types.java. This sharing
creates some awkwardness in Core.java.

## Prerequisites

[GraalVM Community Edition](https://www.graalvm.org/downloads/) (version 20.1.0 or higher)
should be on your PATH and pointed to by JAVA_HOME.

You'll also need to [install Gradle](https://gradle.org/install/)
if you're going to build without using the provided Docker image.

## Outline of Approach

For step 0 through step A, I've purposefully avoided Truffle-specific optimizations.
Step A is intended to be a fully naive application of Truffle, where
a 'pure' interpreter is developed using Truffle AST nodes, but without any attempt
to leverage Truffle primitives to specialize compiled code.

By comparing Truffle step A on OpenJDK to the existing Java step A, we can get a sense of the
overhead imposed by the Truffle framework on interpreter performance.

By comparing Truffle step A on OpenJDK to Truffle step A on GraalVM, we can get a sense of how
much performance the GraalVM compiler can give the language implementor "for free".

Each step _after_ A employs Truffle primitives to enable specialization
of code during compilation.

* Step B specializes function calls by assuming that the same function will
  always be called (i.e. that call sites are _monomorphic_), until proven otherwise.
  At call sites where the same function _actually is_ always called, the compiler
  can eliminate some code and perform inlining.

* Step C optimizes and specializes environment lookups, allowing
  us to avoid HashMap-related overhead for lookups of symbols that are statically in
  scope (i.e. function arguments and let bindings) under the assumption that some
  def! doesn't dynamically bind the looked-up symbols at runtime in scopes where they
  aren't declared.

* Step D enables _further_ specialization of environment lookups for closed-over
  environments, allowing us to skip the lookups entirely under the assumption that
  the symbols have not been rebound.

* Step E specializes macro expansion, allowing the results of a macro expansion to
  _replace_ the apply form entirely. We have to 'cheat' in this step, and extend Mal's
  macro semantics (in a backward-compatible way!). The results are worth it!

## Performance Evaluation Method

Truffle Mal performance is evaluated relative to Java Mal on several benchmarks.
For each benchmark, we run Java Mal and Truffle Mal on both OpenJDK and GraalVM.

```bash
   # OpenJDK
   $ java -version
   openjdk version "11.0.7" 2020-04-14
   OpenJDK Runtime Environment (build 11.0.7+10-post-Ubuntu-2ubuntu218.04)
   OpenJDK 64-Bit Server VM (build 11.0.7+10-post-Ubuntu-2ubuntu218.04, mixed mode, sharing)
   
   # GraalVM
   $ java -version
   openjdk version "11.0.7" 2020-04-14
   OpenJDK Runtime Environment GraalVM CE 20.1.0 (build 11.0.7+10-jvmci-20.1-b02)
   OpenJDK 64-Bit Server VM GraalVM CE 20.1.0 (build 11.0.7+10-jvmci-20.1-b02, mixed mode, sharing)
```

It must be said that Truffle Mal leverage Clojure's implementations of persistent
vectors and maps. This likely has little to no impact on the perf4 and fib benchmarks,
which don't operate on vectors or maps. Self-hosted Mal, however, depends on
the host Mal's map implementation for its environments. Since Java Mal's maps
are built on java.util.HashMap and don't take advantage of structural sharing,
we expect the complexity of Java Mal's assoc and dissoc functions to be strictly
worse than Truffle Mal's ( O(n) versus O(lg(n)) ). Whether or not this actually
tips things in favor of Truffle Mal isn't clear; the sizes of the environments
in question are quite small. I have not made any attempt to account for this
in the results.

### Fib

This simple benchmark focuses on symbol lookups, arithmetic, and function application.
We use the naive recursive approach to computing the 30th Fibonacci number. We run
the computation 10 times, and select the fastest result.

### Busywork

The busywork.mal benchmark is a refactoring of the perf3.mal benchmark,
which primarily tests macro and atom performance.

We measure how long it takes to execute 10,000 iterations of a 'busywork' function.
As with fib.mal, this is done 10 times and we use the fastest result.

### Fib on Mal

For a more interesting test, we run the `fib.mal` benchmark using self-hosted
Mal. This gives each implementation a more comprehensive workout. We compute
the 15th Fibonacci number 10 times, and take the fastest execution time.

Note that self-hosted Mal does not support tail call optimization, and so consumes more
stack the longer it runs. For Truffle Mal, we need to increase the stack size from the
default of 1MB to 8MB to avoid stack overflow.

## Results

Truffle performance is given in absolute terms, and relative to the faster of the
Java implementation's OpenJDK and GraalVM runs for the same benchmark. 

### Step A: No Optimizations

Step A represents a naive Mal interpreter written using Truffle AST nodes, but with
no special effort made to leverage Truffle primitives to assist the GraalVM compiler.

| Benchmark  | Java (OpenJDK) | Truffle (OpenJDK) | Truffle (GraalVM) |
| ---------- | -------------- | ----------------- | ----------------- |
| Fib        | 1700 ms        | 1293 ms (1.3x)    | 675 ms (2.5x)     |
| Busywork   | 781 ms         | 914 ms            | 888 ms            |
| Fib on Mal | 686 ms         | 2101 ms           | 1664 ms           |

On the Fib benchmark, the Java and Truffle implementations of Mal are in the same
ball park on OpenJDK, with Truffle being 1.3x faster. However, when we run the
Truffle implementation on GraalVM, we see nearly a 2x speed-up over OpenJDK effectively
for free, putting it at 2.5x faster than plain old Java.

The Busywork benchmark is a different story, with the Truffle implementation _slightly_
slower on both OpenJDK and GraalVM, and with GraalVM providing very little extra performance.

Fib on Mal is stranger yet: the Truffle implementation is 3x _slower_ on OpenJDK, and GraalVM
doesn't offer much help. What's going on?!

A bit of profiling quickly yields the answer: Macros.

From `truffle.mal.stepA_mal$ApplyNode`:

```java
            if (fn.isMacro) {
                // Mal's macro semantics are... interesting. To preserve them in the
                // general case, we must re-expand a macro each time it's applied.
                // Executing the result means turning it into a Truffle AST, creating
                // a CallTarget, calling it, and then throwing it away.
                // This is TERRIBLE for performance! Truffle should not be used like this!
                var result = applyMacro(env, fn);
                var newRoot = new MalRootNode(language, result, env, invokeNode.tailPosition);
                var target = Truffle.getRuntime().createCallTarget(newRoot);
                return invokeNode.invoke(target, new Object[] {}, false);
            } else {
```

A Truffle `CallTarget` represents an AST that can be called from other code. Call Target construction
is a heavy-weight operation that traverses the entire AST to do various initialization things.
The cost of this is _supposed_ to be amortized over the many calls to the code, and offset by the
gains we see for code that is called often enough to be JIT compiled. Truffle ASTs support self-modification.
Ideally, we'd expand a macro once, and then replace the macro application node with the result.

Mal's macro semantics, alas, prevent us from doing so.
A Mal macro can choose to expand code one way or another based on the current value of any in-scope
environment, or even user input. Even worse, Mal's incremental macro expansion behavior is such that it
is allowable to write 'tail-recursive' macros that would, if eagerly expanded, take up space
exponential in their inputs. Consider a sumdown macro:

```
   (defmacro! sumdown-via-macro* (fn* [acc n]
     `(if (<= ~n 0)
       ~acc
       (sumdown-via-macro* ~(+ acc n) ~(- n 1)))))

   (defmacro! sumdown-via-macro2 (fn* [n]
     `(sumdown-via-macro* 0 ~(eval n))))
```

This executes without issue in any conforming Mal implementation!

We'll return to macros in Step E, but before we do, we'll see what we can specialize
within the confines of Mal's semantics.

### Step B: Specializing Function Calls

In Step A, all function call sites are represented in the AST using Truffle's
`IndirectCallNode`. Truffle also provides a `DirectCallNode` for use at call sites
where the same function is always called. Direct function calls may be inlined by
the GraalVM compiler.

Mal's semantics make it difficult (and sometimes impossible?) to prove statically
that the same function will always be called at a given call site. However, it's
trivial for our interpreter to _assume_ that a call site is direct up until we
learn that it isn't. If we use Truffle properly, we can express this assumption
in a way that the GraalVM compiler understands.

Here's what the Steb B version of `InvokeNode` looks like:

```java

    static class InvokeNode extends AbstractInvokeNode {
        final boolean tailPosition;
        @CompilationFinal private boolean initialized = false;
        @CompilationFinal private boolean usingCachedTarget;
        @CompilationFinal private CallTarget cachedTarget;
        @CompilationFinal @Child private DirectCallNode directCallNode;
        @CompilationFinal @Child private IndirectCallNode indirectCallNode;

        /* SNIP */

        Object invoke(CallTarget target, Object[] args, boolean allowTailCall) {
            if (tailPosition && allowTailCall) {
                throw new TailCallException(target, args);
            } else {
                if (!initialized) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    initialized = true;
                    usingCachedTarget = true;
                    cachedTarget = target;
                    directCallNode = Truffle.getRuntime().createDirectCallNode(target);
                }
                while (true) {
                    try {
                        if (usingCachedTarget) {
                            if (cachedTarget == target) {
                                return directCallNode.call(args);
                            }
                            CompilerDirectives.transferToInterpreterAndInvalidate();
                            usingCachedTarget = false;
                            indirectCallNode = Truffle.getRuntime().createIndirectCallNode();
                        }
                        return indirectCallNode.call(target, args);
                    } catch (TailCallException ex) {
                        target = ex.callTarget;
                        args = ex.args;
                    }
                }
            }
        }
    }
```

It _looks_ like it should be slower now, with all the branching. What have we done?

Notice that all the new member variables have been annotated with `@CompilationFinal`.
This tells the compiler to treat these variables as if they were `final`, because their values
will not change in compiled code.

We _ensure_ that they do not change in compiled code
by inserting the `CompilerDirectives.transferToInterpreterAndInvalidate()` intrinsic.
In interpreted code, this is a no-op. In _compiled_ code, it is replaced with an instruction
that causes the compiler to _de-optimize_ the compiled code and return to the interpreter
to continue execution.

Suppose a function containing a call site that is not in tail position has been executed
enough times to trigger compilation,
and each time the invoked function has been the same. When compilation kicks in, the
variables `initialized` and `usingCachedTarget` would be true, and `tailPosition` would
be false.
Accordingly, the invoke code simplifies to:

```java
        Object invoke(CallTarget target, Object[] args, boolean allowTailCall) {
            while (true) {
                try {
                    if (cachedTarget == target) {
                        return directCallNode.call(args);
                    }
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                } catch (TailCallException ex) {
                    target = ex.callTarget;
                    args = ex.args;
                }
            }
        }
```

Much better!

Because we're using a `DirectCallNode`, the compiler might decide to inline the called
function as well. Function inlining allows the partial evaluation algorithm to extend
across function boundaries.

Let's see if there's an improvement in practice...

| Benchmark  | Java (OpenJDK) | Truffle (OpenJDK) | Truffle (GraalVM) |
| ---------- | -------------- | ----------------- | ----------------- |
| Fib        | 1700 ms        | 991 ms (1.7x)     | 430 ms (3.9x)     |
| Busywork   | 781 ms         | 671 ms (1.2x)     | 409 ms (1.9x)     |
| Fib on Mal | 686 ms         | 1912 ms (0.35x)   | 1407 ms (0.48x)   |

We see modest improvements over Step A in all cases, with the Busywork benchmark
having a 2x improvement over Step A on GraalVM.

### Step C: Static Symbol Lookup

A little profiling shows that quite a lot of the 'work' that goes into executing a Mal
program is just environment maintenance: constructing HashMaps, putting symbol/value pairs
into them, and looking them back up again. For code that does a lot of function calling
(like our Fib benchmark), this adds up to a lot of overhead.

Why do we need the HashMaps at all? Why can't we build environments around Object arrays?
During construction of an AST from a Mal form, we can keep track of the variables in
each lexical scope, and assign each one a _slot_ (an index in the Object array for the
environment associated with that scope). During execution, we can construct
environments out of Object arrays, and get/set values using these slots. No more
HashMaps! Right?

The trouble, of course, is that `def!` can mutate environments at runtime, adding
bindings for symbols that were never 'declared' via `let*` or `fn*`. Consider this
function:

```
   (def! f (fn* [x b] (do (who-knows? b) y)))
```

The symbol `y` isn't lexically in scope, so we wouldn't assign
it a slot; we'd have to try to look it up in the global environment
at execution time. But what if, at execution time, `who-knows?` turns out
to resolve to a _macro_ like:

```
   (fn* [b] (if b `(def! y 42)))
```

If `b` is truthy, the `y` symbol ends up bound in the function body's environment after all,
but there's no slot for it in the environment's object array. Drat!

But the power of Truffle is that we don't _need_ to statically prove that our slot
assignments and usage are valid. We're not writing a compiler! Instead, we can just
_assume_ that the slot assignments we make are valid, right up until we find that they
aren't. Then we can fall back on a less efficient but more general approach.

I won't elaborate much on the details of the code too much in step, it involves the most significant changes.
At a high level, here's what we do:

* Introduce a `LexicalScope` class that assigns symbols to array indices, and
  thread `LexicalScope` objects through our AST construction methods.
* Extend `MalEnv` with a `staticBindings` Object array _in addition to_ the normal
  `bindings` HashMap. The Object array is constructed based on the number of symbols in
  the associated `LexicalScope`. The `bindings` HashMap is only constructed _lazily_,
  if a symbol that isn't in a `LexicalScope` is bound via a `def!`.
* Further extend `MalEnv` with slot-based `get` and `set` methods, in addition to the
  existing symbol-based `get` and `set` methods.
* Extend the AST nodes for `let*` and `fn*` to introduce new `LexicalScope` objects
  with the right symbols, assign slots to those symbols,
  and use the slot-based `get` and `set` methods on `MalEnv` to bind symbols.
* Modify the AST node for symbol lookups to speculatively use slot-based lookups
  when the symbol in question is in a lexical scope _under the assumption that it has
  not been re-defined via `def!`.

That last bit is the key to the whole thing: We use Truffle's `Assumption` abstraction
to tell the compiler about the assumption that our slot-based symbol look-ups depend on.
When a `LexicalScope` assigns a slot, it creates an `Assumption` that the symbol
has not been bound by `def!` in that or any outer `LexicalScope`. The slot-based
symbol lookup code is guarded by that assumption. The 'dynamic' `set` method of `MalEnv`
(the one used by `def!`) is modified to _invalidate_ that assumption, triggering
de-optimization of any symbol lookups that might have been rendered incorrect.

After slot assignment, where do we stand?

| Benchmark  | Java (OpenJDK) | Truffle (OpenJDK) | Truffle (GraalVM) |
| ---------- | -------------- | ----------------- | ----------------- |
| Fib        | 1700 ms        | 829 ms (2.1x)     | 219 ms (7.8x)     |
| Busywork   | 781 ms         | 686 ms (1.1x)     | 394 ms (2.0x)     |
| Fib on Mal | 686 ms         | 1932 ms (0.35x)   | 1507 ms (0.46x)   |

This optimization starts to show off the real power of the Truffle framework on GraalVM,
at least for the Fib benchmark.
On the JDK, we see a modest improvement (1.2x) over Step B that comes from eliminating
some of the HashMap overhead. Given the complexity that we had to introduce,
this isn't very satisfying, On GraalVM, though, we see a better than 2x speed-up, taking
us to almost 8x faster than the Java interpreter.

However, the other two benchmarks show no meaningful improvement at all. Fib on Mal
even seems to have become slower! Once again, we're bit by macros here. Recall that
since we currently create a new AST each and every time we
encounter a macro, the compiler never has a chance to compile it. We pay all the overhead
of our extra book-keeping, and get absolutely no benefit.

### Step D: Caching Symbol Lookups

We can take the symbol lookup improvements much further, now that we've laid the groundwork!

Symbol lookups for symbols that are declared in some lexical scope will now use the fast-path Object array
lookups instead of the HashMap lookups, and Truffle _should_ even be able to unroll the loops
that walk up the chain of environments for us. For local symbol lookups, we probably won't do
much better.

But what about symbols in a function body that _aren't_ lexically in scope? In a well-behaved
Mal program that isn't doing anything fancy with `def!`, these symbols will either produce
runtime environments, or resolve to the global environment. In practice, they're almost always
looking up core functions, whose values are unlikely (but not impossible!) to change over
the lifetime of the program.

We can _further_ specialize symbol lookups by simply caching looked-up values for symbols
that are not lexically in scope, and _skipping subsequent lookups entirely_ unless the
looked-up symbol gets rebound. Once again, we create an `Assumption` for each cached
lookup to represent that we assume it has not been redefined, update `def!` to invalidate
that assumption.

| Benchmark  | Java (OpenJDK) | Truffle (OpenJDK) | Truffle (GraalVM) |
| ---------- | -------------- | ----------------- | ----------------- |
| Fib        | 1700 ms        | 733 ms (2.3x)     | 18 ms (94x !!)    |
| Busywork   | 781 ms         | 657 ms (1.2x)     | 311 ms (2.5x)     |
| Fib on Mal | 686 ms         | 1971 ms (0.35x)   | 1474 ms (0.47x)   |

On our Fib benchmark, caching symbol lookups makes a _huge_ difference. Look at the
code for `fib`:

```
(def! fib (fn* [n]
  (if (= n 0)
    1
    (if (= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))))
```

There are 7 look-ups of symbols not in lexical scope (`=`, `=`, `+`, `fib`, `-`, `fib`, and `-`),
and we've effectively eliminated all of them. All that's left are fast slot-based lookups for `n`,
two comparisons, and three arithmetic operations. All of those end up getting inlined by the compiler.
Moreover, the compiler actually 'unrolls' the recorsion several levels for us by inlining `fib` into itself.
The result is quite fast, even out-performing type-hinted Clojure (Mal's inspiration)... on OpenJDK, anyway.

Alas, the macros still defeat us on the other benchmarks, for the same reasons. The time has come to do something
about that.

### Step E: Macro Inlining

If we stay within the confines of Mal's semantics, macros are a
show-stopper performance killer for us. Mal's
macro semantics are just too dynamic for their own good. Sure, you _can_
write tail recursive macros... but why _would_ you?

In practice, macros are often just introducing 'syntactic sugar' to improve expressiveness.
Consider the macros `cond`, `or`, `and`, `->`, and `->>`. Their
expansion behavior does not depend on runtime values (so they expand the same
way on each application), and they produce code that is linear in the size of
their inputs.

Why do all the work to re-expand them on every application? Why not expand them _once_,
and then just substitute the result? Clojure macros, for example, work this way.

To make further progress, we're going to have to "cheat" our way into fast macros.
We extend Mal's semantics
such that a macro with a map for metadata containing the entry `:inline? true`
is expanded once, and the result is _inlined_ in place of the macro application
forever after. We then mark all of the above macros as inlined macros.

This isn't a Truffle-specific optimization by any means. Any Mal interpreter
that supports these semantics will see _substantial_ performance gains. However,
the immutable nature of Mal data structures might make the refactoring of
these interpreters a bit trickier than we'd like.

Using Truffle, though, it's a trivial change. Truffle ASTs are explicitly self-modifying.
It boils down to this:

```java
            if (fn.isMacro) {
                var expanded = applyMacro(env, fn);
                if (isInlinableMacro(fn)) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    var newNode = expanded.body;
                    this.replace(newNode);
                    return newNode.executeGeneric(frame, env);
                } else {
                    return invokeMacro(expanded);
                }
            else {
```

A few extra lines is all it takes. Look what happens now...

| Benchmark  | Java (OpenJDK) | Truffle (OpenJDK) | Truffle (GraalVM) |
| ---------- | -------------- | ----------------- | ----------------- |
| Fib        | 1700 ms        | 718 ms (2.3x)     | 21 ms (81x)       |
| Busywork   | 781 ms         | 19 ms (41x)       | 12 ms (65x)       |
| Fib on Mal | 686 ms         | 104 ms (6.6x)     | 25 ms (27x)       |

No substantial difference on Fib, which makes sense: that benchmark doesn't use macros.

_Huge_ gains on Busywork and Fib on Mal, because both are so dependent on macros.
It's a bit suspicious, though, that there isn't more of a performance difference between
the OpenJDK and GraalVM runs. Maybe the test runs so fast we're not sufficiently warmed up?
Let's crank up the number of iterations from 10k to 100k and see what happens.


| Benchmark    | Java (OpenJDK) | Truffle (OpenJDK) | Truffle (GraalVM) |
| ------------ | -------------- | ----------------- | ----------------- |
| Busywork 10x | 7264 ms        | 223 ms (32x)      | 37 ms (196x)      |

That's more like it. Recall that before this macro optimization, Java
and Java Truffle were close in performance. If we implemented macro inlining
in Java Mal, to make for a fair comparison, it's still likely that Truffle Mal
wins by around 6-7x, which is pretty decent!

What about the Fib on Mal benchmark? Why don't we see a bigger difference
between the OpenJDK and GraalVM runs? It's not insufficient warm-up this time.
Doing some profiling shows that we're spending quite a bit of time in
code that isn't partially evaluated. For example, self-hosted Mal's environment
implementation turns symbols into strings, and uses the strings as
keys in environment maps, instead of just using the symbols themselves.
The code for turning objects into strings in Printer depends heavily on
JDK-provided classes that were not designed with partial evaluation in mind,
so we must exclude them from partial evaluation to avoid an explosion in
code size.

## Conclusions

Does Truffle deliver on the promise of high-performance JIT-ed code via
partial evaluation of interpreter code? Based on my experience, it certainly does.
It's not exactly magic pixie dust that gives you 100x for free, but it
doesn't claim to be. It _does_ enable order-of-magnitude speed improvements
over plain old interpreters with _much_ less than an order-of-magnitude
increase in effort.

Let's revisit the questions we started with:

*Do more complicated Mal programs show similar speed-ups?*

No, GraalVM JIT compilation does not provide arbitrary Mal programs with the
massive performance gains we see on the Fib benchmark. This should be
totally unsurprising.

*How much of the speed-up is really attributable to the Truffle/GraalVM combo,
and how much came from optimizations that could be applied to any Mal interpreter?*

Our benchmarks show that the answer depends heavily on the nature of the
program. Let's look at the performance of Truffle Mal on GraalVM relative
to its performance on OpenJDK (where we don't have the benefit of Truffle-
enabled partial evaluation):

| Benchmark    | TruffleMal (GraalVM relative to OpenJDK) |
| ------------ | ---------------------------------------- |
| Fib          | 34x |
| Busywork 10x | 6x  |
| Fib On Mal   | 4x  |

In extreme cases, for programs that are heavy on arithmetic and function calls,
our use of Truffle/GraalVM buys us 30x _after accounting for our optimizations_.

That's pretty amazing.

Realistically, though, we're likely to see more 3-6x speed-ups directly attributable
to Truffle/GraalVM. Still impressive!

*How much simplicity did we have to sacrifice in the name of performance?*

Let's look at the size, in lines of code, of each implementation.

| File           | LOC (Java) | LOC (Truffle Step A) | LOC (Truffle Step E) |
| -------------- | ---------- | -------------------- | -------------------- |
| stepA_mal.java | 310        | 757                  | 886                  |
| env.java       | 58         | 145                  | 370                  |
| printer.java   | 53         | 100                  | 100                  |
| reader.java    | 151        | 166                  | 166                  |
| types.java     | 381        | 532                  | 545                  |
| core.java      | 633        | 1506                 | 1511                 |
| *Total*        | 1586       | 3206 (2x)            | 3578 (2.25x)         |

The Truffle-based implementation, before optimizations, weighs in at about
2x the size of the Java implementation.
Much of this can be attributed to 'boilerplate' associated with use of the Truffle framework.
In my opinion, this boilerplate adds effectively nothing to the conceptual complexity of
the implementation. In fact, much of the extra weight comes from the core functions.
The LOC count is longer because we make use of the Truffle DSL, a feature not covered in
this write-up, to trivially allow specialization of core functions based on argument type.
I would argue that while this increases code _size_, it may actually _reduce_ code complexity
via a form of pattern matching.

Our specializations to the interpreter nodes themselves added about 15%, or 120 lines.
More significantly, we increased the size of the environment implementation by 2.5x,
adding substantial complexity in the process.

*Was it worth it?*

This is both totally subjective and a gross over-simplification,
but let's just guess that we've increased the complexity of the baseline Java interpreter
overall by roughly 1.5 x, and environments in particular by 3x.
In exchange for this increase in complexity, we've managed to obtain between from 25x to 80x
better performance over the baseline Java interpreter, depending on the Mal
program.

We could perform most of our optimizations on that Java interpreter _without_
using Truffle. However, we'd end up at a similar level of complexity, and
would see substantially smaller performance gains.

Based on these results, if I were to attempt a 'production quality' Mal implementation,
I'd probably do it with Truffle and GraalVM. The performance gains alone seem to justify it.

It's also worth observing that the Truffle/GraalVM provide _other_ interesting benefits
that are not performance-related. I won't cover them here. I think the most interesting
non-performance benefit is the promise of interoperability with other Truffle languages.

## Bonus: AOT-compiled Mal

GraalVM can ahead-of-time compile Java into a stand-alone executable (with some caveats)
called a _native image_.
This works even for Truffle interpreters! With AOT-compiled Mal, we get all the JIT compilation
goodness of Truffle, _and_ we ditch the need for a Java runtime, **and** we skip the long JVM
start-up time! A GraalVM native image of our Mal interpreter is well suited for scripts and
command line applications.

The `make-native.sh` script can be used to compile a native image of any Mal step.
To run it, though, you'll need some additional
[prerequisites](https://www.graalvm.org/reference-manual/native-image/#prerequisites).

The `make-native.sh` script

* assumes you've already run `gradle build` to compile all Java classes
* takes as its only argument a step name, e.g. `step3_env`
** when no argument is supplied, `stepE_macros` is selected by default
* produces a `build/${STEP}` native image

