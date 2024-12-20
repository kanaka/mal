# General Implementation Notes

This document contains notes on the jq implementation, describing the deviations from the MAL specification and implementation details where necessary.

## Main Deviations per Step

### Step 0
As jq lacks a way to input free-form data on-demand, the REPL is implemented using a wrapper around the jq interpreter, which intercepts requests from our implementation and feeds the result back to jq as JSON; see the `__readline` function in [utils.jq](../utils.jq), and its implementation in [the wrapper](../run).

All further free-form I/O primitives are implemented in a similar way.

### Step 1
There is not much deviation from the MAL process in this step, MAL data are implemented as JSON objects with two fields: `kind` and `value` (see [reader.jq](../reader.jq)).

### Step 2
jq cannot store functions as values, and so we are forced to represent them using their names and a large switch-case structure (`select()` in jq).
The environment is simply modelled as a JSON object, and functions are represented as `{ "kind": "fn", "inputs": n, "function": name }` where `n` is the number of arguments the function takes and `name` is the name of the function to be handled by the switch-case structure (in `interpret()` at this stage).

### Step 3
The second of three environment implementations is introduced here, where an environment is an optional parent environment (which corresponds to the `outer` environment concept in the guide), and the environment from the previous step. Two convenience functions are introduced to handle the environment operations: `env-get` and `env-set`.

The forms `let*` and `def!` are implemented mostly as described in the guide, with `let*` utilizing a left-associative fold (`reduce` in jq) to build the intermediate environment up; which is discarded after the fold is done.

### Step 4
In this step, environments grow yet another field `fallback`, which is used to add a second environment chain to non-top-level environments. This is used to implement functions that refer to unbound symbols in their body (this could be the function itself, or any other symbol defined later in the parent environment) - this is necessary as there are no variable references or mutable variables in jq (and thus we cannot modify an environment in-place).

Due to this limitation, the `fn*` form is implemented by:
- Recording the "free" symbols in the function body (which are not defined in the function's environment)
- And storing a copy of the current environment in the function itself (for closures)

The `interpret` function also gets an `_eval` callback parameter, which is used to evaluate the function body after a new environment is created with the correct bindings.

Everything else is largely the same as in the guide.

### Step 5
Tail-calls are implemented as a (fairly complex) fixpoint iteration in the `EVAL` function; this "loop" takes an object of the form `{ast, env, ret_env, finish, cont}` and "iteratively" performs an evaluation step with `.ast` and `.env` (which is updated on every "iteration") until `.cont` is `false` (which is driven by the `finish` "flag"). Upon completion, the resulting environment is pulled from `ret_env` and the fixpoint is returned as the evaluation result.

This is largely due to the lack of "actual" loops in jq, a computation of this form can also be expressed as a reduction over an infinite generator, but the fixpoint iteration is more straightforward to implement (as jq has a built-in `recurse` function).

### Step 6
This step deviates from the guide _significantly_, in the implementation of atoms; since jq does not have mutable variables (_or_ global variables), we cannot implement atoms in any simple way.

First, let's go over atom identity and creation; this implementation "stamps" atoms with their creation timestamp (the result of `now | tostring`), which is used as a unique identifier for the atom.
The fixpoint calculation of `EVAL` (and `TCOWrap` in particular) is adjusted to handle atoms "leaking" into the global environment (as they are not bound to any environment in reality, which differs from our implementation where atoms are bound to the active environment they were created in).

The `interpret` function is also moved to a separate [interp.jq](../interp.jq) file, as it can be shared between steps going forward, and will also grow in complexity due to the introduction of atoms.

### Step 7
This step does not deviate from the guide.

### Step 8
This step does not deviate from the guide.

### Step 9
This step uses the native jq exception handling mechanism `try ... catch ...`, and follows the guide closely (and so no significant deviations are present).

### Step A
This step does not deviate from the guide.
