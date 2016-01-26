#
# Command line settings
#

MAL_IMPL = js

PYTHON = python
USE_MATLAB =
# python, js, cpp, or neko are currently supported
HAXE_MODE = neko

# Extra options to pass to runtest.py
TEST_OPTS =

# Test with previous test files not just the test files for the
# current step. Step 0 and 1 tests are special and not included in
# later steps.
REGRESS=

# Extra implementation specific options to pass to runtest.py
mal_TEST_OPTS = --start-timeout 60 --test-timeout 120

#
# Settings
#

IMPLS = awk bash c d clojure coffee cpp crystal cs erlang elixir es6 \
	factor forth fsharp go groovy guile haskell haxe java julia \
	js kotlin lua make mal ocaml matlab miniMAL nim perl php ps \
	python r racket rpython ruby rust scala swift tcl vb vimscript

step0 = step0_repl
step1 = step1_read_print
step2 = step2_eval
step3 = step3_env
step4 = step4_if_fn_do
step5 = step5_tco
step6 = step6_file
step7 = step7_quote
step8 = step8_macros
step9 = step9_try
stepA = stepA_mal

regress_step0 = step0
regress_step1 = step1
regress_step2 = step2
regress_step3 = $(regress_step2) step3
regress_step4 = $(regress_step3) step4
regress_step5 = $(regress_step4) step5
regress_step6 = $(regress_step5) step6
regress_step7 = $(regress_step6) step7
regress_step8 = $(regress_step7) step8
regress_step9 = $(regress_step8) step9
regress_stepA = $(regress_step9) stepA

STEP5_EXCLUDES += awk     # completes at 10,000
STEP5_EXCLUDES += bash    # no stack exhaustion or completion
STEP5_EXCLUDES += c       # segfault
STEP5_EXCLUDES += cpp     # completes at 10,000
STEP5_EXCLUDES += cs      # fatal stack overflow fault
STEP5_EXCLUDES += d       # completes at 10,000, fatal stack overflow at 1,000,000
STEP5_EXCLUDES += erlang  # erlang is TCO, test passes
STEP5_EXCLUDES += elixir  # elixir is TCO, test passes
STEP5_EXCLUDES += fsharp  # completes at 10,000, fatal stack overflow at 100,000
STEP5_EXCLUDES += haskell # test completes
STEP5_EXCLUDES += make    # no TCO capability/step
STEP5_EXCLUDES += mal     # no TCO capability/step
STEP5_EXCLUDES += matlab  # too slow to complete 10,000
STEP5_EXCLUDES += miniMAL # strange error with runtest.py
STEP5_EXCLUDES += nim     # test completes, even at 100,000
STEP5_EXCLUDES += go      # test completes, even at 100,000
STEP5_EXCLUDES += php     # test completes, even at 100,000
STEP5_EXCLUDES += racket  # test completes
STEP5_EXCLUDES += ruby    # test completes, even at 100,000
STEP5_EXCLUDES += rust    # no catching stack overflows
STEP5_EXCLUDES += ocaml   # test completes, even at 1,000,000
STEP5_EXCLUDES += vb      # completes at 10,000
STEP5_EXCLUDES += crystal # test completes, even at 1,000,000

PERF_EXCLUDES = mal  # TODO: fix this

#
# Utility functions
#

MATLAB = matlab -nodisplay -nosplash -nodesktop -nojvm -r
OCTAVE = octave --no-gui -q --traditional --eval
matlab_args = $(subst $(SPACE),$(COMMA),$(foreach x,$(strip $(1)),'$(x)'))
matlab_cmd = $(if $(strip $(USE_MATLAB)),$(MATLAB),$(OCTAVE))

haxe_STEP_TO_PROG_neko   = haxe/$($(1)).n
haxe_STEP_TO_PROG_python = haxe/$($(1)).py
haxe_STEP_TO_PROG_cpp    = haxe/cpp/$($(1))
haxe_STEP_TO_PROG_js     = haxe/$($(1)).js

haxe_RUNSTEP_neko   = neko ../$(2) $(3)
haxe_RUNSTEP_python = python3 ../$(2) $(3)
haxe_RUNSTEP_cpp    = ../$(2) $(3)
haxe_RUNSTEP_js     = node ../$(2) $(3)

# Return list of test files for a given step. If REGRESS is set then
# test files will include step 2 tests through tests for the step
# being tested.
STEP_TEST_FILES = $(strip $(wildcard \
		    $(foreach s,$(if $(strip $(REGRESS)),$(regress_$(2)),$(2)),\
		      $(1)/tests/$($(s)).mal tests/$($(s)).mal)))

# Map of step (e.g. "step8") to executable file for that step
awk_STEP_TO_PROG =     awk/$($(1)).awk
bash_STEP_TO_PROG =    bash/$($(1)).sh
c_STEP_TO_PROG =       c/$($(1))
d_STEP_TO_PROG =       d/$($(1))
clojure_STEP_TO_PROG = clojure/src/$($(1)).clj
coffee_STEP_TO_PROG =  coffee/$($(1)).coffee
cpp_STEP_TO_PROG =     cpp/$($(1))
crystal_STEP_TO_PROG = crystal/$($(1))
cs_STEP_TO_PROG =      cs/$($(1)).exe
elixir_STEP_TO_PROG =  elixir/lib/mix/tasks/$($(1)).ex
erlang_STEP_TO_PROG =  erlang/$($(1))
es6_STEP_TO_PROG =     es6/build/$($(1)).js
factor_STEP_TO_PROG =  factor/$($(1))/$($(1)).factor
forth_STEP_TO_PROG =   forth/$($(1)).fs
fsharp_STEP_TO_PROG =  fsharp/$($(1)).exe
go_STEP_TO_PROG =      go/$($(1))
groovy_STEP_TO_PROG =  groovy/$($(1)).groovy
java_STEP_TO_PROG =    java/target/classes/mal/$($(1)).class
haskell_STEP_TO_PROG = haskell/$($(1))
haxe_STEP_TO_PROG =    $(haxe_STEP_TO_PROG_$(HAXE_MODE))
julia_STEP_TO_PROG =   julia/$($(1)).jl
js_STEP_TO_PROG =      js/$($(1)).js
kotlin_STEP_TO_PROG =  kotlin/$($(1)).jar
lua_STEP_TO_PROG =     lua/$($(1)).lua
make_STEP_TO_PROG =    make/$($(1)).mk
mal_STEP_TO_PROG =     mal/$($(1)).mal
ocaml_STEP_TO_PROG =   ocaml/$($(1))
matlab_STEP_TO_PROG =  matlab/$($(1)).m
miniMAL_STEP_TO_PROG = miniMAL/$($(1)).json
nim_STEP_TO_PROG =     nim/$($(1))
perl_STEP_TO_PROG =    perl/$($(1)).pl
php_STEP_TO_PROG =     php/$($(1)).php
ps_STEP_TO_PROG =      ps/$($(1)).ps
python_STEP_TO_PROG =  python/$($(1)).py
r_STEP_TO_PROG =       r/$($(1)).r
racket_STEP_TO_PROG =  racket/$($(1)).rkt
rpython_STEP_TO_PROG = rpython/$($(1))
ruby_STEP_TO_PROG =    ruby/$($(1)).rb
rust_STEP_TO_PROG =    rust/target/release/$($(1))
scala_STEP_TO_PROG =   scala/$($(1)).scala
swift_STEP_TO_PROG =   swift/$($(1))
tcl_STEP_TO_PROG =     tcl/$($(1)).tcl
vb_STEP_TO_PROG =      vb/$($(1)).exe
vimscript_STEP_TO_PROG = vimscript/$($(1)).vim
guile_STEP_TO_PROG =   guile/$($(1)).scm


# Needed some argument munging
COMMA = ,
noop =
SPACE = $(noop) $(noop)
export FACTOR_ROOTS := .

# Macro for running a step:
#   $(1): step (e.g. "stepA")
#   $(2): program for step (e.g. result of *_STEP_TO_PROG
#   $(3): program arguments
awk_RUNSTEP =     awk -O -f ../$(2) $(3)
bash_RUNSTEP =    bash ../$(2) $(3)
c_RUNSTEP =       ../$(2) $(3)
d_RUNSTEP =       ../$(2) $(3)
clojure_RUNSTEP = lein with-profile +$(1) trampoline run $(3)
coffee_RUNSTEP =  coffee ../$(2) $(3)
cpp_RUNSTEP =     ../$(2) $(3)
crystal_RUNSTEP = ../$(2) $(3)
cs_RUNSTEP =      mono ../$(2) --raw $(3)
elixir_RUNSTEP =  mix $(notdir $(basename $(2))) $(3)
erlang_RUNSTEP =  ../$(2) $(3)
es6_RUNSTEP =     node ../$(2) $(3)
factor_RUNSTEP =  factor ../$(2) $(3)
forth_RUNSTEP =   gforth ../$(2) $(3)
fsharp_RUNSTEP =  mono ../$(2) --raw $(3)
go_RUNSTEP =      ../$(2) $(3)
groovy_RUNSTEP =  groovy ../$(2) $(3)
haskell_RUNSTEP = ../$(2) $(3)
haxe_RUNSTEP =    python3 ../$(2) $(3)
haxe_RUNSTEP =    $(haxe_RUNSTEP_$(HAXE_MODE))
java_RUNSTEP =    mvn -quiet exec:java -Dexec.mainClass="mal.$($(1))" $(if $(3), -Dexec.args="$(3)",)
julia_RUNSTEP =   ../$(2) $(3)
js_RUNSTEP =      node ../$(2) $(3)
kotlin_RUNSTEP =  java -jar ../$(2) $(3)
lua_RUNSTEP =     ../$(2) $(3)
make_RUNSTEP =    make -f ../$(2) $(3)
mal_RUNSTEP =     $(call $(MAL_IMPL)_RUNSTEP,stepA,$(call $(MAL_IMPL)_STEP_TO_PROG,stepA),../$(2),")  #"
ocaml_RUNSTEP =   ../$(2) $(3)
matlab_RUNSTEP =  $(matlab_cmd) "$($(1))($(call matlab_args,$(3)));quit;"
miniMAL_RUNSTEP = miniMAL ../$(2) $(3)
nim_RUNSTEP =     ../$(2) $(3)
perl_RUNSTEP =    perl ../$(2) $(3)
php_RUNSTEP =     php ../$(2) $(3)
ps_RUNSTEP =      gs -q -I./ -dNODISPLAY -- ../$(2) $(3)
python_RUNSTEP =  $(PYTHON) ../$(2) $(3)
r_RUNSTEP =       Rscript ../$(2) $(3)
racket_RUNSTEP =  ../$(2) $(3)
rpython_RUNSTEP = ../$(2) $(3)
ruby_RUNSTEP =    ruby ../$(2) $(3)
rust_RUNSTEP =    ../$(2) $(3)
scala_RUNSTEP =   sbt 'run-main $($(1))$(if $(3), $(3),)'
swift_RUNSTEP =   ../$(2) $(3)
tcl_RUNSTEP =     tclsh ../$(2) --raw $(3)
vb_RUNSTEP =      mono ../$(2) --raw $(3)
vimscript_RUNSTEP = ./run_vimscript.sh ../$(2) $(3)
# needs TERM=dumb to work with readline
guile_RUNSTEP =   guile --no-auto-compile -L ../guile ../$(2) $(3)


vimscript_TEST_OPTS = --test-timeout 30
ifeq ($(MAL_IMPL),vimscript)
mal_TEST_OPTS = --start-timeout 60 --test-timeout 180
endif

# Derived lists
STEPS = $(sort $(filter step%,$(.VARIABLES)))
DO_IMPLS = $(filter-out $(SKIP_IMPLS),$(IMPLS))
IMPL_TESTS = $(foreach impl,$(DO_IMPLS),test^$(impl))
STEP_TESTS = $(foreach step,$(STEPS),test^$(step))
ALL_TESTS = $(filter-out $(foreach impl,$(STEP5_EXCLUDES),test^$(impl)^step5),\
              $(strip $(sort \
                $(foreach impl,$(DO_IMPLS),\
                  $(foreach step,$(STEPS),test^$(impl)^$(step))))))

IMPL_STATS = $(foreach impl,$(DO_IMPLS),stats^$(impl))
IMPL_STATS_LISP = $(foreach impl,$(DO_IMPLS),stats-lisp^$(impl))

DOCKER_BUILD = $(foreach impl,$(DO_IMPLS),docker-build^$(impl))

IMPL_PERF = $(foreach impl,$(filter-out $(PERF_EXCLUDES),$(DO_IMPLS)),perf^$(impl))

IMPL_REPL = $(foreach impl,$(DO_IMPLS),repl^$(impl))
ALL_REPL = $(strip $(sort \
             $(foreach impl,$(DO_IMPLS),\
               $(foreach step,$(STEPS),repl^$(impl)^$(step)))))

#
# Build rules
#

# Build a program in an implementation directory
# Make sure we always try and build first because the dependencies are
# encoded in the implementation Makefile not here
.PHONY: $(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(call $(i)_STEP_TO_PROG,$(s))))
$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(call $(i)_STEP_TO_PROG,$(s)))):
	$(foreach impl,$(word 1,$(subst /, ,$(@))),\
	  $(MAKE) -C $(impl) $(subst $(impl)/,,$(@)))

# Allow test, test^STEP, test^IMPL, and test^IMPL^STEP
.SECONDEXPANSION:
$(IMPL_TESTS): $$(filter $$@^%,$$(ALL_TESTS))

.SECONDEXPANSION:
$(STEP_TESTS): $$(foreach step,$$(subst test^,,$$@),$$(filter %^$$(step),$$(ALL_TESTS)))

.SECONDEXPANSION:
$(ALL_TESTS): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  $(foreach step,$(word 3,$(subst ^, ,$(@))),\
	    cd $(if $(filter mal,$(impl)),$(MAL_IMPL),$(impl)); \
	    $(foreach test,$(call STEP_TEST_FILES,$(impl),$(step)),\
	      echo '----------------------------------------------'; \
	      echo 'Testing $@, step file: $+, test file: $(test)'; \
	      echo 'Running: ../runtest.py $(TEST_OPTS) $(call $(impl)_TEST_OPTS) ../$(test) -- $(call $(impl)_RUNSTEP,$(step),$(+))'; \
	      ../runtest.py $(TEST_OPTS) $(call $(impl)_TEST_OPTS) ../$(test) -- $(call $(impl)_RUNSTEP,$(step),$(+));)))

test: $(ALL_TESTS)
tests: $(ALL_TESTS)


# Stats rules

stats: $(IMPL_STATS)
stats-lisp: $(IMPL_STATS_LISP)

.SECONDEXPANSION:
$(IMPL_STATS):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Stats for $(impl):"; \
	  $(MAKE) --no-print-directory -C $(impl) stats)

.SECONDEXPANSION:
$(IMPL_STATS_LISP):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Stats (lisp only) for $(impl):"; \
	  $(MAKE) --no-print-directory -C $(impl) stats-lisp)

# Docker build rules

docker-build: $(DOCKER_BUILD)

.SECONDEXPANSION:
$(DOCKER_BUILD):
	echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Running: docker build -t kanaka/mal-test-$(impl) .:"; \
	  cd $(impl) && docker build -t kanaka/mal-test-$(impl) .)

# Performance test rules

perf: $(IMPL_PERF)

.SECONDEXPANSION:
$(IMPL_PERF):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  cd $(if $(filter mal,$(impl)),$(MAL_IMPL),$(impl)); \
	  echo "Performance test for $(impl):"; \
	  echo 'Running: $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf1.mal)'; \
          $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf1.mal); \
	  echo 'Running: $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf2.mal)'; \
          $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf2.mal); \
	  echo 'Running: $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf3.mal)'; \
          $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf3.mal))

# REPL invocation rules
# Allow repl^IMPL^STEP and repl^IMPL (which starts REPL of stepA)

.SECONDEXPANSION:
$(IMPL_REPL): $$@^stepA

.SECONDEXPANSION:
$(ALL_REPL): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  $(foreach step,$(word 3,$(subst ^, ,$(@))),\
	    cd $(if $(filter mal,$(impl)),$(MAL_IMPL),$(impl)); \
	    echo 'REPL implementation $(impl), step file: $+'; \
	    echo 'Running: $(call $(impl)_RUNSTEP,$(step),$(+))'; \
	    $(call $(impl)_RUNSTEP,$(step),$(+));))
