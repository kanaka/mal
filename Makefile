#
# Command line settings
#

MAL_IMPL = js

#
# Settings
#

IMPLS = bash c clojure cs java js make mal php ps python ruby

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

EXCLUDE_TESTS = test^make^step5 test^mal^step0 test^mal^step5 test^mal^step9 test^java^step9 test^cs^step9

#
# Utility functions
#

STEP_TEST_FILES = $(strip $(wildcard $(1)/tests/$($(2)).mal) $(wildcard tests/$($(2)).mal))

bash_STEP_TO_PROG = bash/$($(1)).sh
c_STEP_TO_PROG = c/$($(1))
clojure_STEP_TO_PROG = clojure/src/$($(1)).clj
cs_STEP_TO_PROG = cs/$($(1)).exe
java_STEP_TO_PROG = java/src/main/java/mal/$($(1)).java
js_STEP_TO_PROG = js/$($(1)).js
make_STEP_TO_PROG = make/$($(1)).mk
mal_STEP_TO_PROG = mal/$($(1)).mal
php_STEP_TO_PROG = php/$($(1)).php
ps_STEP_TO_PROG = ps/$($(1)).ps
python_STEP_TO_PROG = python/$($(1)).py
ruby_STEP_TO_PROG = ruby/$($(1)).rb


bash_RUNTEST = ../runtest.py $(4) ../$(1) -- bash ../$(2) $(5)
c_RUNTEST = ../runtest.py $(4) ../$(1) -- ../$(2) $(5)
clojure_RUNTEST = ../runtest.py $(4) ../$(1) -- lein with-profile +$(3) trampoline run $(5)
cs_RUNTEST = ../runtest.py --redirect $(4) ../$(1) -- mono --debug ../$(2) --raw $(5)
java_RUNTEST = ../runtest.py $(4) ../$(1) -- mvn -quiet exec:java -Dexec.mainClass="mal.$($(3))" -Dexec.args="--raw$(if $(5), $(5),)"
js_RUNTEST = ../runtest.py $(4) ../$(1) -- node ../$(2) $(5)
make_RUNTEST = ../runtest.py $(4) ../$(1) -- make -f ../$(2) $(5)
mal_RUNTEST = $(call $(MAL_IMPL)_RUNTEST,$(1),$(call $(MAL_IMPL)_STEP_TO_PROG,stepA),stepA,--start-timeout 30 --test-timeout 120,../$(2))
php_RUNTEST = ../runtest.py $(4) ../$(1) -- php ../$(2) $(5)
ps_RUNTEST = ../runtest.py $(4) ../$(1) -- gs -q -dNODISPLAY ../$(2) $(5)
python_RUNTEST = ../runtest.py $(4) ../$(1) -- python ../$(2) $(5)
ruby_RUNTEST = ../runtest.py $(4) ../$(1) -- ruby ../$(2) $(5)


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
	      echo 'Running: $(call $(impl)_RUNTEST,$(test),$(+),$(step))'; \
	      $(call $(impl)_RUNTEST,$(test),$(+),$(step)))))

test: $(ALL_TESTS)
tests: $(ALL_TESTS)


# Stats rules

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

stats: $(IMPL_STATS)
stats-lisp: $(IMPL_STATS_LISP)
