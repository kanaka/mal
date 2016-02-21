# Mal/Make-a-Lisp FAQ

<a name="why_mal"></a>

### Why did you create mal/make-a-lisp?
### OR Why the name "mal"?
### OR Why?
### OR Wat?

In November of 2013, Alan Dipert gave a [lightning talk at
Clojure/conj](https://www.youtube.com/watch?v=bmHTFo2Rf2w#t=28m55s)
about [gherkin](https://github.com/alandipert/gherkin), a Lisp
implemented in bash. His presentation led me to ask myself the qestion
of whether a Lisp could be created using the GNU Make macro language.
As you have probably guessed, the answer to that question is yes.

Interestingly, the current pedagogical/educational purpose of mal
happened due to a semantic naming accident (naming is such a fraught
task in computer science). If I am remembering correctly, the name
"mal" original meant "MAke Lisp". I do not remember precisely why
I continued to create more implementations, apart from the fact that
it was a fun challenge, but after the make implementation, many of the
others were relatively easy. At some point during that process,
I realized that the multiple implementations and incremental steps
(which was originally just for my own clarity) was a useful learning
tool and so the "mal" name became a double entendre for "Make, A Lisp"
and "make-a-lisp" (and eventually just the latter given that the make
implementation is now just a small part of the whole).


<a name="code_split"></a>

### Why is some code split into steps and some code not?

The split between code that goes in steps and code that goes into other files
is not completely arbitrary (a bit arbitrary, but not completely). My rule of
thumb is something like this: if the code is specific and necessary for
implementing a Lisp then it belongs in the step files. If the purpose of the
code is for implementing new dynamic data-types/objects and the functions or
methods that operate on those types, then it goes in separate files.

If the target language has types and functions that resemble mal types, then
those files tend to be very small or non-existent. Examples:

* the mal implementation has no types, reader, printer files and
  has a trivial core file (just to hoist underlying functions)
* the Clojure implementation has no types file and fairly trivial
  reader and printer files (just to modify the Clojure reader/writer
  slightly) and a fairly trivial core file
* ruby types and the functions that operate on them are very "Lispy"
  so the Ruby types file and core file are very small.

The env file is somewhat more arbitrary, however, it is
a self-contained module that is implemented early and changes very
little after that, so I decided to separate it. Also, for languages
that have hierarchical maps/dictionaries (e.g. Javascript
objects/prototype chain), you do not necessarily need an env file.

Another way of summarizing this answer is that the step files
represent the core of what makes something a Lisp, the rest of the
modules are just language specific details (they may be the harder
than the Lisp part, but that is due to the nature of the target
language not because of Lisp functionality per se).


<a name="steps"></a>

### Why are the mal/make-a-lisp steps structured the way they are?

### OR Why is X functionality in step Y instead of step Z?

There is no single consistent rule that I have used to determine which
functionality goes in which step and the arrangement has changed
numerous times since the beginning of the project. There are several
different goals that I try and balance in determining which
functionality goes into which step:

* **Optimize Lisp learning**: I want developers who are unfamiliar with
  Lisp to be able to use the project and guide to learn about Lisp
  without becoming overwhelmed. In many Lisp introductions, concepts
  like quoting and homoiconicity (i.e. a user exposed eval function)
  are introduced early. But these are fairly foreign to most other
  languages so they are introduced in later steps in mal. I also try
  to not to concentrate too many Lisp concepts in a single step. So
  many steps contain one or two Lisp concepts plus some core function
  additions that support those concepts.

* **Optimize implementation language learning (equal-ish step
  sizing)**: I try to structure the steps so that the target
  implementation can be learned incrementally. This goal is the one
  that has caused me to refactor the steps the most. Different
  languages have different areas that they optimize and make simple
  for the developer. For example, in Java (prior to 8) and PostScript
  creating the equivalent of anonymous functions and function closures
  is painful. In other languages, function closures are trivial, but
  IO and error handling are tedious when you are first learning the
  language (I am looking at you Haskell). So this goal is really about
  trying to balance step size across multiple languages.

* **Practical results early and continuous feedback**: it is
  a scientific fact that many small rewards are more motivating than
  a single large reward (citation intentionally omitted, get a small
  reward by googling it yourself). Each step in mal adds new
  functionality that can actually be exercised by the implementor and,
  just as importantly, easily tested.

Also, the step structure of mal/make-a-lisp is not perfect. It never
will be perfect, but there are some areas that could be improved. The
most glaring problem is that step1 is on the heavy/large size because
in most languages you have to implement a good portion of the
reader/printer before you can begin using/testing the step. The
compromise I have settled on for now is to put extra detail in the
process guide for step1 and to be clear that many of the types are
deferrable until later. But I am always open to suggestions.


<a name="add_implementation"></a>

### Will you add my new implementation?

Absolutely! I want mal to have a idiomatic implementation in every
programming language.

Here are a few guidelines for getting your implementation accepted
into the main repository:

* Your implementation needs to be complete enough to self-host. This
  means that all the tests should pass in both direct and self-hosted modes:
  ```bash
  make "test^[IMPL_NAME]"
  make MAL_IMPL=[IMPL_NAME] "test^mal"
  ```
  You do not need to pass the final optional tests for stepA that are
  marked as optional and not needed for self-hosting.

* Your implementation should follow the existing mal steps and
  structure: Lisp-centric code (eval, eval_ast, quasiquote,
  macroexpand) in the step files, other code in reader, printer, env,
  and core files. See [code layout rationale](#code_split) above.
  I encourage you to create implementations that take mal in new
  directions for your own learning and experimentation, but for it to
  be included in the main repository I ask that it follows the steps
  and structure.

* Your implementation should stick as much as possible to the accepted
  idioms and conventions in that language. Try to create an
  implementation that will not make an expert in that language say
  "Woah, that's a strange way of doing things". And on that topic,
  I make no guarantees that the existing implementations are
  particularly idiomatic in their target languages (improvements are
  welcome). However, if it is clear to me that your implementation is
  not idiomatic in a given language then I will probably ask you to
  improve it first.
   
* If you are creating a new implementation for an existing
  implementation (or somebody beats you to the punch while you are
  working on it), there is still a chance I will merge your
  implementation. If you can make a compelling argument that your
  implementation is more idiomatic or significantly better than the
  existing implementation then I may replace the existing one.
  However, if your approach is different or unique from the existing
  implementation, there is still a good chance I will merge your
  implementation side-by-side with the existing one. In that case
  I will add your github username as a suffix to the language
  implementation directory. At the very least, even if I decide not to
  merge your implementation, I am certainly willing to link to you
  implementation once it is completed.

* You do not need to implement line editing (i.e. readline)
  functionality for your implementation, however, it is a nice
  convenience for users of your implementation and I personally find
  it saves a lot of time when I am creating a new implementation to
  have line edit support early on in the REPL.
