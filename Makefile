#
# Command line settings
#

MAL_IMPL = js

PYTHON = python

#
# Settings
#

IMPLS = awk bash c clojure coffee cpp crystal cs erlang elixir es6 factor forth fsharp go groovy \
	haskell java julia js lua make mal ocaml matlab miniMAL nim \
	perl php ps python r racket rpython ruby rust scala swift vb guile

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

EXCLUDE_TESTS += test^awk^step5 # completes at 10,000
EXCLUDE_TESTS += test^bash^step5 # no stack exhaustion or completion
EXCLUDE_TESTS += test^c^step5    # segfault
EXCLUDE_TESTS += test^cpp^step5  # completes at 10,000
EXCLUDE_TESTS += test^cs^step5   # fatal stack overflow fault
EXCLUDE_TESTS += test^erlang^step5 # erlang is TCO, test passes
EXCLUDE_TESTS += test^elixir^step5 # elixir is TCO, test passes
EXCLUDE_TESTS += test^fsharp^step5 # completes at 10,000, fatal stack overflow at 100,000
EXCLUDE_TESTS += test^haskell^step5 # test completes
EXCLUDE_TESTS += test^make^step5 # no TCO capability/step
EXCLUDE_TESTS += test^mal^step5  # no TCO capability/step
EXCLUDE_TESTS += test^miniMAL^step5 # strange error with runtest.py
EXCLUDE_TESTS += test^nim^step5   # test completes, even at 100,000
EXCLUDE_TESTS += test^go^step5   # test completes, even at 100,000
EXCLUDE_TESTS += test^php^step5  # test completes, even at 100,000
EXCLUDE_TESTS += test^racket^step5 # test completes
EXCLUDE_TESTS += test^ruby^step5 # test completes, even at 100,000
EXCLUDE_TESTS += test^rust^step5 # no catching stack overflows
EXCLUDE_TESTS += test^ocaml^step5 # test completes, even at 1,000,000
EXCLUDE_TESTS += test^vb^step5   # completes at 10,000
EXCLUDE_TESTS += test^crystal^step5   # test completes, even at 1,000,000

EXCLUDE_PERFS = perf^mal  # TODO: fix this

#
# Utility functions
#

STEP_TEST_FILES = $(strip $(wildcard $(1)/tests/$($(2)).mal) $(wildcard tests/$($(2)).mal))

awk_STEP_TO_PROG =     awk/$($(1)).awk
bash_STEP_TO_PROG =    bash/$($(1)).sh
c_STEP_TO_PROG =       c/$($(1))
clojure_STEP_TO_PROG = clojure/src/$($(1)).clj
coffee_STEP_TO_PROG =  coffee/$($(1)).coffee
cpp_STEP_TO_PROG =     cpp/$($(1))
crystal_STEP_TO_PROG = crystal/$($(1))
cs_STEP_TO_PROG =      cs/$($(1)).exe
elixir_STEP_TO_PROG =  elixir/lib/mix/tasks/$($(1)).ex
erlang_STEP_TO_PROG =  erlang/$($(1))
es6_STEP_TO_PROG =     es6/build/$($(1)).js
factor_STEP_TO_PROG =  factor/src/$($(1))/$($(1)).factor
forth_STEP_TO_PROG =   forth/$($(1)).fs
fsharp_STEP_TO_PROG =  fsharp/$($(1)).exe
go_STEP_TO_PROG =      go/$($(1))
groovy_STEP_TO_PROG =  groovy/$($(1)).groovy
java_STEP_TO_PROG =    java/src/main/java/mal/$($(1)).java
haskell_STEP_TO_PROG = haskell/$($(1))
julia_STEP_TO_PROG =   julia/$($(1)).jl
js_STEP_TO_PROG =      js/$($(1)).js
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
vb_STEP_TO_PROG =      vb/$($(1)).exe
guile_STEP_TO_PROG =   guile/$($(1)).scm

# Needed some argument munging
COMMA = ,
noop =
SPACE = $(noop) $(noop)
export FACTOR_ROOTS := src

awk_RUNSTEP =     awk -O -f ../$(2) $(3)
bash_RUNSTEP =    bash ../$(2) $(3)
c_RUNSTEP =       ../$(2) $(3)
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
java_RUNSTEP =    mvn -quiet exec:java -Dexec.mainClass="mal.$($(1))" $(if $(3), -Dexec.args="$(3)",)
julia_RUNSTEP =   ../$(2) $(3)
js_RUNSTEP =      node ../$(2) $(3)
lua_RUNSTEP =     ../$(2) $(3)
make_RUNSTEP =    make -f ../$(2) $(3)
mal_RUNSTEP =     $(call $(MAL_IMPL)_RUNSTEP,$(1),$(call $(MAL_IMPL)_STEP_TO_PROG,stepA),../$(2),")  #"
ocaml_RUNSTEP =   ../$(2) $(3)
matlab_args =     $(subst $(SPACE),$(COMMA),$(foreach x,$(strip $(1)),'$(x)'))
matlab_RUNSTEP =  matlab -nodisplay -nosplash -nodesktop -nojvm -r "$($(1))($(call matlab_args,$(3)));quit;"
miniMAL_RUNSTEP = miniMAL ../$(2) $(3)
nim_RUNSTEP =     ../$(2) $(3)
perl_RUNSTEP =    perl ../$(2) $(3)
php_RUNSTEP =     php ../$(2) $(3)
ps_RUNSTEP =      $(4)gs -q -I./ -dNODISPLAY -- ../$(2) $(3)$(4)
python_RUNSTEP =  $(PYTHON) ../$(2) $(3)
r_RUNSTEP =       Rscript ../$(2) $(3)
racket_RUNSTEP =  ../$(2) $(3)
rpython_RUNSTEP = ../$(2) $(3)
ruby_RUNSTEP =    ruby ../$(2) $(3)
rust_RUNSTEP =    ../$(2) $(3)
scala_RUNSTEP =   sbt 'run-main $($(1))$(if $(3), $(3),)'
swift_RUNSTEP =   ../$(2) $(3)
vb_RUNSTEP =      mono ../$(2) --raw $(3)
# needs TERM=dumb to work with readline
guile_RUNSTEP =   guile -L ../guile ../$(2) $(3)

# Extra options to pass to runtest.py
mal_TEST_OPTS = --start-timeout 60 --test-timeout 120


# Derived lists
STEPS = $(sort $(filter step%,$(.VARIABLES)))
DO_IMPLS = $(filter-out $(SKIP_IMPLS),$(IMPLS))
IMPL_TESTS = $(foreach impl,$(DO_IMPLS),test^$(impl))
STEP_TESTS = $(foreach step,$(STEPS),test^$(step))
ALL_TESTS = $(filter-out $(EXCLUDE_TESTS),\
              $(strip $(sort \
                $(foreach impl,$(DO_IMPLS),\
                  $(foreach step,$(STEPS),test^$(impl)^$(step))))))

IMPL_STATS = $(foreach impl,$(DO_IMPLS),stats^$(impl))
IMPL_STATS_LISP = $(foreach impl,$(DO_IMPLS),stats-lisp^$(impl))

IMPL_PERF = $(filter-out $(EXCLUDE_PERFS),$(foreach impl,$(DO_IMPLS),perf^$(impl)))

#
# Build rules
#

# Build a program in an implementation directory
$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(call $(i)_STEP_TO_PROG,$(s)))):
	$(MAKE) -C $(dir $(@)) $(notdir $(@))

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
	      echo 'Running: ../runtest.py $(call $(impl)_TEST_OPTS) ../$(test) -- $(call $(impl)_RUNSTEP,$(step),$(+))'; \
	      ../runtest.py $(call $(impl)_TEST_OPTS) ../$(test) -- $(call $(impl)_RUNSTEP,$(step),$(+));)))

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
