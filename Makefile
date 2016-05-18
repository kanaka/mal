# Usage/help
all help:
	@echo
	@echo 'USAGE:'
	@echo
	@echo 'Rules/Targets:'
	@echo
	@echo 'make "IMPL"                       # build all steps of IMPL'
	@echo 'make "IMPL^STEP"                  # build STEP of IMPL'
	@echo
	@echo 'make "test"                       # test all implementations'
	@echo 'make "test^IMPL"                  # test all steps of IMPL'
	@echo 'make "test^STEP"                  # test STEP for all implementations'
	@echo 'make "test^IMPL^STEP"             # test STEP of IMPL'
	@echo
	@echo 'make "perf"                       # run microbenchmarks for all implementations'
	@echo 'make "perf^IMPL"                  # run microbenchmarks for IMPL'
	@echo
	@echo 'make "repl^IMPL"                  # run stepA of IMPL'
	@echo 'make "repl^IMPL^STEP"             # test STEP of IMPL'
	@echo
	@echo 'make "clean"                      # run 'make clean' for all implementations'
	@echo 'make "clean^IMPL"                 # run 'make clean' for IMPL'
	@echo
	@echo 'make "stats"                      # run 'make stats' for all implementations'
	@echo 'make "stats-lisp"                 # run 'make stats-lisp' for all implementations'
	@echo 'make "stats^IMPL"                 # run 'make stats' for IMPL'
	@echo 'make "stats-lisp^IMPL"            # run 'make stats-lisp' for IMPL'
	@echo
	@echo 'Options/Settings:'
	@echo
	@echo 'make MAL_IMPL=IMPL "test^mal..."  # use IMPL for self-host tests'
	@echo 'make REGRESS=1 "test..."          # test with previous step tests too'
	@echo 'make DOCKERIZE=1 ...              # to dockerize above rules/targets'
	@echo
	@echo 'Other:'
	@echo
	@echo 'make "docker-build^IMPL"          # build docker image for IMPL'
	@echo

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
REGRESS =

DEFERRABLE=1
OPTIONAL=1

# Extra implementation specific options to pass to runtest.py
mal_TEST_OPTS = --start-timeout 60 --test-timeout 120
miniMAL_TEST_OPTS = --start-timeout 60 --test-timeout 120
plpgsql_TEST_OPTS = --start-timeout 60 --test-timeout 180

DOCKERIZE=

# Run target/rule within docker image for the implementation
DOCKERIZE =

#
# Settings
#

IMPLS = ada awk bash c d clojure coffee cpp crystal cs erlang elisp \
	elixir es6 factor forth fsharp go groovy guile haskell haxe \
	io java julia js kotlin lua make mal ocaml matlab miniMAL \
	nim objc objpascal perl php plpgsql ps python r racket \
	rpython ruby rust scala swift swift3 tcl vb vhdl vimscript

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

test_EXCLUDES += test^bash^step5   # never completes at 10,000
test_EXCLUDES += test^make^step5   # no TCO capability (iteration or recursion)
test_EXCLUDES += test^mal^step5    # host impl dependent
test_EXCLUDES += test^matlab^step5 # never completes at 10,000
test_EXCLUDES += test^plpgsql^step5    # too slow for 10,000

perf_EXCLUDES = mal  # TODO: fix this

dist_EXCLUDES += mal
# TODO: still need to implement dist
dist_EXCLUDES += guile io julia matlab swift

#
# Utility functions
#

haxe_STEP_TO_PROG_neko   = haxe/$($(1)).n
haxe_STEP_TO_PROG_python = haxe/$($(1)).py
haxe_STEP_TO_PROG_cpp    = haxe/cpp/$($(1))
haxe_STEP_TO_PROG_js     = haxe/$($(1)).js

opt_DEFERRABLE      = $(if $(strip $(DEFERRABLE)),$(if $(filter t true T True TRUE 1 y yes Yes YES,$(DEFERRABLE)),--deferrable,--no-deferrable),--no-deferrable)
opt_OPTIONAL        = $(if $(strip $(OPTIONAL)),$(if $(filter t true T True TRUE 1 y yes Yes YES,$(OPTIONAL)),--optional,--no-optional),--no-optional)

# Return list of test files for a given step. If REGRESS is set then
# test files will include step 2 tests through tests for the step
# being tested.
STEP_TEST_FILES = $(strip $(wildcard \
		    $(foreach s,$(if $(strip $(REGRESS)),$(regress_$(2)),$(2)),\
		      $(1)/tests/$($(s)).mal tests/$($(s)).mal)))

# Map of step (e.g. "step8") to executable file for that step
ada_STEP_TO_PROG =     ada/$($(1))
awk_STEP_TO_PROG =     awk/$($(1)).awk
bash_STEP_TO_PROG =    bash/$($(1)).sh
c_STEP_TO_PROG =       c/$($(1))
d_STEP_TO_PROG =       d/$($(1))
clojure_STEP_TO_PROG = clojure/target/$($(1)).jar
coffee_STEP_TO_PROG =  coffee/$($(1)).coffee
cpp_STEP_TO_PROG =     cpp/$($(1))
crystal_STEP_TO_PROG = crystal/$($(1))
cs_STEP_TO_PROG =      cs/$($(1)).exe
elisp_STEP_TO_PROG =   elisp/$($(1)).el
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
io_STEP_TO_PROG =      io/$($(1)).io
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
objc_STEP_TO_PROG =    objc/$($(1))
objpascal_STEP_TO_PROG = objpascal/$($(1))
perl_STEP_TO_PROG =    perl/$($(1)).pl
php_STEP_TO_PROG =     php/$($(1)).php
plpgsql_STEP_TO_PROG = plpgsql/$($(1)).sql
ps_STEP_TO_PROG =      ps/$($(1)).ps
python_STEP_TO_PROG =  python/$($(1)).py
r_STEP_TO_PROG =       r/$($(1)).r
racket_STEP_TO_PROG =  racket/$($(1)).rkt
rpython_STEP_TO_PROG = rpython/$($(1))
ruby_STEP_TO_PROG =    ruby/$($(1)).rb
rust_STEP_TO_PROG =    rust/target/release/$($(1))
scala_STEP_TO_PROG =   scala/target/scala-2.11/classes/$($(1)).class
swift_STEP_TO_PROG =   swift/$($(1))
swift3_STEP_TO_PROG =  swift3/$($(1))
tcl_STEP_TO_PROG =     tcl/$($(1)).tcl
vb_STEP_TO_PROG =      vb/$($(1)).exe
vhdl_STEP_TO_PROG =    vhdl/$($(1))
vimscript_STEP_TO_PROG = vimscript/$($(1)).vim
guile_STEP_TO_PROG =   guile/$($(1)).scm


# Needed some argument munging
COMMA = ,
noop =
SPACE = $(noop) $(noop)
export FACTOR_ROOTS := .

# DOCKERIZE utility functions
lc = $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$1))))))))))))))))))))))))))
impl_to_image = kanaka/mal-test-$(call lc,$(1))

actual_impl = $(if $(filter mal,$(1)),$(MAL_IMPL),$(1))

# Takes impl
# Returns nothing if DOCKERIZE is not set, otherwise returns the
# docker prefix necessary to run make within the docker environment
# for this impl
get_build_prefix = $(if $(strip $(DOCKERIZE)),docker run -it --rm -u $(shell id -u) -v $(dir $(abspath $(lastword $(MAKEFILE_LIST)))):/mal -w /mal/$(1) $(if $(filter factor,$(1)),-e FACTOR_ROOTS=$(FACTOR_ROOTS),) $(call impl_to_image,$(1)) ,)

# Takes impl and step arguments
# Returns a command prefix (docker command and environment variables)
# necessary to launch the given impl and step
get_run_prefix = $(strip $(if $(strip $(DOCKERIZE)),\
    docker run -e STEP=$($2) \
    -it --rm -u $(shell id -u) \
    -v $(dir $(abspath $(lastword $(MAKEFILE_LIST)))):/mal \
    -w /mal/$(call actual_impl,$(1)) \
    $(if $(filter haxe,$(1)),-e HAXE_MODE=$(HAXE_MODE),) \
    $(if $(filter factor,$(1)),-e FACTOR_ROOTS=$(FACTOR_ROOTS),) \
    $(foreach env,$(3),-e $(env)) \
    $(call impl_to_image,$(call actual_impl,$(1))) \
    ,\
    env STEP=$($2) \
    $(if $(filter haxe,$(1)),HAXE_MODE=$(HAXE_MODE),) \
    $(if $(filter factor,$(1)),FACTOR_ROOTS=$(FACTOR_ROOTS),) \
    $(3)))

# Takes impl and step
# Returns the runtest command prefix (with runtest options) for testing the given step
get_runtest_cmd = $(call get_run_prefix,$(1),$(2),$(if $(filter cs fsharp tcl vb,$(1)),RAW=1,)) \
		    ../runtest.py $(TEST_OPTS) $(opt_DEFERRABLE) $(opt_OPTIONAL) $(call $(1)_TEST_OPTS)

# Takes impl and step
# Returns the runtest command prefix (with runtest options) for testing the given step
get_argvtest_cmd = $(call get_run_prefix,$(1),$(2)) ../run_argv_test.sh

vimscript_TEST_OPTS = --test-timeout 30
ifeq ($(MAL_IMPL),vimscript)
mal_TEST_OPTS = --start-timeout 60 --test-timeout 180
endif

# Derived lists
STEPS = $(sort $(filter step%,$(.VARIABLES)))
DO_IMPLS = $(filter-out $(SKIP_IMPLS),$(IMPLS))
IMPL_TESTS = $(foreach impl,$(DO_IMPLS),test^$(impl))
STEP_TESTS = $(foreach step,$(STEPS),test^$(step))
ALL_TESTS = $(filter-out $(test_EXCLUDES),\
              $(strip $(sort \
                $(foreach impl,$(DO_IMPLS),\
                  $(foreach step,$(STEPS),test^$(impl)^$(step))))))

DOCKER_BUILD = $(foreach impl,$(DO_IMPLS),docker-build^$(impl))

IMPL_PERF = $(foreach impl,$(filter-out $(perf_EXCLUDES),$(DO_IMPLS)),perf^$(impl))

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
	  $(if $(DOCKERIZE), \
	    $(call get_build_prefix,$(impl))$(MAKE) $(patsubst $(impl)/%,%,$(@)), \
	    $(MAKE) -C $(impl) $(subst $(impl)/,,$(@))))

# Allow IMPL, and IMPL^STEP
.SECONDEXPANSION:
$(DO_IMPLS): $$(foreach s,$$(STEPS),$$(call $$(@)_STEP_TO_PROG,$$(s)))

.SECONDEXPANSION:
$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(i)^$(s))): $$(call $$(word 1,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 2,$$(subst ^, ,$$(@))))


#
# Test rules
#

.SECONDEXPANSION:
$(ALL_TESTS): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  $(foreach step,$(word 3,$(subst ^, ,$(@))),\
	    cd $(if $(filter mal,$(impl)),$(MAL_IMPL),$(impl)) && \
	    $(foreach test,$(call STEP_TEST_FILES,$(impl),$(step)),\
	      echo '----------------------------------------------' && \
	      echo 'Testing $@; step file: $+, test file: $(test)' && \
	      echo 'Running: $(call get_runtest_cmd,$(impl),$(step)) $(call $(impl)_TEST_OPTS) ../$(test) -- ../$(impl)/run' && \
	      $(call get_runtest_cmd,$(impl),$(step)) $(call $(impl)_TEST_OPTS) ../$(test) -- ../$(impl)/run && \
	      $(if $(filter tests/step6_file.mal,$(test)),\
	        echo '----------------------------------------------' && \
	        echo 'Testing ARGV of $@; step file: $+' && \
	        echo 'Running: $(call get_argvtest_cmd,$(impl),$(step)) ../$(impl)/run ' && \
	        $(call get_argvtest_cmd,$(impl),$(step)) ../$(impl)/run  && ,\
		true && ))\
	    true))

# Allow test, tests, test^STEP, test^IMPL, and test^IMPL^STEP
test: $(ALL_TESTS)
tests: $(ALL_TESTS)

.SECONDEXPANSION:
$(IMPL_TESTS): $$(filter $$@^%,$$(ALL_TESTS))

.SECONDEXPANSION:
$(STEP_TESTS): $$(foreach step,$$(subst test^,,$$@),$$(filter %^$$(step),$$(ALL_TESTS)))


#
# Dist rules
#

dist: $(IMPL_DIST)

.SECONDEXPANSION:
$(IMPL_DIST):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Running: make -C $(impl) dist"; \
	  $(MAKE) --no-print-directory -C $(impl) dist)


#
# Docker build rules
#

docker-build: $(DOCKER_BUILD)

.SECONDEXPANSION:
$(DOCKER_BUILD):
	echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Running: docker build -t $(call impl_to_image,$(impl)) .:"; \
	  cd $(impl) && docker build -t $(call impl_to_image,$(impl)) .)


#
# Performance test rules
#

perf: $(IMPL_PERF)

.SECONDEXPANSION:
$(IMPL_PERF):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  cd $(if $(filter mal,$(impl)),$(MAL_IMPL),$(impl)); \
	  echo "Performance test for $(impl):"; \
	  echo 'Running: $(call get_run_prefix,$(impl),stepA) ../$(impl)/run ../tests/perf1.mal'; \
	  $(call get_run_prefix,$(impl),stepA) ../$(impl)/run ../tests/perf1.mal; \
	  echo 'Running: $(call get_run_prefix,$(impl),stepA) ../$(impl)/run ../tests/perf2.mal'; \
	  $(call get_run_prefix,$(impl),stepA) ../$(impl)/run ../tests/perf2.mal; \
	  echo 'Running: $(call get_run_prefix,$(impl),stepA) ../$(impl)/run ../tests/perf3.mal'; \
	  $(call get_run_prefix,$(impl),stepA) ../$(impl)/run ../tests/perf3.mal)


#
# REPL invocation rules
#

.SECONDEXPANSION:
$(ALL_REPL): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  $(foreach step,$(word 3,$(subst ^, ,$(@))),\
	    cd $(if $(filter mal,$(impl)),$(MAL_IMPL),$(impl)); \
	    echo 'REPL implementation $(impl), step file: $+'; \
	    echo 'Running: $(call get_run_prefix,$(impl),$(step)) ../$(impl)/run'; \
	    $(call get_run_prefix,$(impl),$(step)) ../$(impl)/run;))

# Allow repl^IMPL^STEP and repl^IMPL (which starts REPL of stepA)
.SECONDEXPANSION:
$(IMPL_REPL): $$@^stepA

#
# Utility functions
#
.SECONDEXPANSION:
print-%:
	@echo "$($(*))"

#
# Recursive rules (call make FOO in each subdirectory)
#

define recur_template
.PHONY: $(1)
$(1): $(2)
.SECONDEXPANSION:
$(2):
	@echo "----------------------------------------------"; \
	$$(foreach impl,$$(word 2,$$(subst ^, ,$$(@))),\
	  $$(if $$(DOCKERIZE), \
	    echo "Running: $$(call get_build_prefix,$$(impl))$$(MAKE) --no-print-directory $(1)"; \
	    $$(call get_build_prefix,$$(impl))$$(MAKE) --no-print-directory $(1), \
	    echo "Running: $$(MAKE) --no-print-directory -C $$(impl) $(1)"; \
	    $$(MAKE) --no-print-directory -C $$(impl) $(1)))
endef

recur_impls_ = $(filter-out $(foreach impl,$($(1)_EXCLUDES),$(1)^$(impl)),$(foreach impl,$(IMPLS),$(1)^$(impl)))

# recursive clean
$(eval $(call recur_template,clean,$(call recur_impls_,clean)))

# recursive stats
$(eval $(call recur_template,stats,$(call recur_impls_,stats)))
$(eval $(call recur_template,stats-lisp,$(call recur_impls_,stats-lisp)))

# recursive dist
$(eval $(call recur_template,dist,$(call recur_impls_,dist)))
