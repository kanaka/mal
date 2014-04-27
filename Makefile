#
# Command line settings
#

MAL_IMPL = js

PYTHON = python

#
# Settings
#

IMPLS = bash c clojure cs java js make mal perl php ps python ruby

step0 = step0_repl
step1 = step1_read_print
step2 = step2_eval
step3 = step3_env
step4 = step4_if_fn_do
step5 = step5_tco
step6 = step6_file
step7 = step7_quote
step8 = step8_macros
step9 = step9_interop
stepA = stepA_more

EXCLUDE_TESTS += test^bash^step5 # no stack exhaustion or completion
EXCLUDE_TESTS += test^c^step5    # segfault
EXCLUDE_TESTS += test^cs^step5   # fatal stack overflow fault
EXCLUDE_TESTS += test^make^step5 # no TCO capability/step
EXCLUDE_TESTS += test^mal^step5  # no TCO capability/step
EXCLUDE_TESTS += test^php^step5  # test completes, even at 100,000
EXCLUDE_TESTS += test^ruby^step5 # test completes, even at 100,000

# interop tests now implemented yet
EXCLUDE_TESTS += test^cs^step9 test^java^step9 test^mal^step9 \
		 test^php^step9 test^ps^step9 test^python^step9 \
		 test^ruby^step9

EXCLUDE_PERFS = perf^mal  # TODO: fix this

#
# Utility functions
#

STEP_TEST_FILES = $(strip $(wildcard $(1)/tests/$($(2)).mal) $(wildcard tests/$($(2)).mal))

bash_STEP_TO_PROG =    bash/$($(1)).sh
c_STEP_TO_PROG =       c/$($(1))
clojure_STEP_TO_PROG = clojure/src/$($(1)).clj
cs_STEP_TO_PROG =      cs/$($(1)).exe
java_STEP_TO_PROG =    java/src/main/java/mal/$($(1)).java
js_STEP_TO_PROG =      js/$($(1)).js
make_STEP_TO_PROG =    make/$($(1)).mk
mal_STEP_TO_PROG =     mal/$($(1)).mal
perl_STEP_TO_PROG =    perl/$($(1)).pl
php_STEP_TO_PROG =     php/$($(1)).php
ps_STEP_TO_PROG =      ps/$($(1)).ps
python_STEP_TO_PROG =  python/$($(1)).py
ruby_STEP_TO_PROG =    ruby/$($(1)).rb


bash_RUNSTEP =    bash ../$(2) $(3)
c_RUNSTEP =       ../$(2) $(3)
clojure_RUNSTEP = lein with-profile +$(1) trampoline run $(3)
cs_RUNSTEP =      mono ../$(2) --raw $(3)
java_RUNSTEP =    mvn -quiet exec:java -Dexec.mainClass="mal.$($(1))" -Dexec.args="--raw$(if $(3), $(3),)"
js_RUNSTEP =      node ../$(2) $(3)
make_RUNSTEP =    make -f ../$(2) $(3)
mal_RUNSTEP =     $(call $(MAL_IMPL)_RUNSTEP,$(1),$(call $(MAL_IMPL)_STEP_TO_PROG,stepA),../$(2),")  #"
perl_RUNSTEP =    perl ../$(2) $(3)
php_RUNSTEP =     php ../$(2) $(3)
ps_RUNSTEP =      $(4)gs -q -dNODISPLAY -- ../$(2) $(3)$(4)
python_RUNSTEP =  $(PYTHON) ../$(2) $(3)
ruby_RUNSTEP =    ruby ../$(2) $(3)

# Extra options to pass to runtest.py
cs_TEST_OPTS =  --redirect
mal_TEST_OPTS = --start-timeout 60 --test-timeout 120


# Derived lists
STEPS = $(sort $(filter step%,$(.VARIABLES)))
IMPL_TESTS = $(foreach impl,$(IMPLS),test^$(impl))
STEP_TESTS = $(foreach step,$(STEPS),test^$(step))
ALL_TESTS = $(filter-out $(EXCLUDE_TESTS),\
              $(strip $(sort \
                $(foreach impl,$(IMPLS),\
                  $(foreach step,$(STEPS),test^$(impl)^$(step))))))

IMPL_STATS = $(foreach impl,$(IMPLS),stats^$(impl))
IMPL_STATS_LISP = $(foreach impl,$(IMPLS),stats-lisp^$(impl))

IMPL_PERF = $(filter-out $(EXCLUDE_PERFS),$(foreach impl,$(IMPLS),perf^$(impl)))

#
# Build rules
#

# Build a program in 'c' directory
c/%:
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
	      ../runtest.py $(call $(impl)_TEST_OPTS) ../$(test) -- $(call $(impl)_RUNSTEP,$(step),$(+)))))

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
          $(call $(impl)_RUNSTEP,stepA,$(call $(impl)_STEP_TO_PROG,stepA),../tests/perf2.mal))

