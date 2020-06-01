# Usage/help
all help:
	@echo
	@echo 'USAGE:'
	@echo
	@echo 'Rules/Targets:'
	@echo
	@echo 'make "IMPL"                       # build all steps of IMPL'
	@echo 'make "build^IMPL"                 # build all steps of IMPL'
	@echo 'make "IMPL^STEP"                  # build STEP of IMPL'
	@echo 'make "build^IMPL^STEP"            # build STEP of IMPL'
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
	@echo 'make TEST_OPTS="--opt ..."        # options to pass to runtest.py'
	@echo
	@echo 'Other:'
	@echo
	@echo 'make "docker-build^IMPL"          # build docker image for IMPL'
	@echo
	@echo 'make "docker-shell^IMPL"          # start bash shell in docker image for IMPL'
	@echo

#
# Command line settings
#

MAL_IMPL = js

# cbm or qbasic
basic_MODE = cbm
# clj or cljs (Clojure vs ClojureScript/lumo)
clojure_MODE = clj
# gdc, ldc2, or dmd
d_MODE = gdc
# python, js, cpp, or neko
haxe_MODE = neko
# octave or matlab
matlab_MODE = octave
# python, python2 or python3
python_MODE = python
# scheme (chibi, kawa, gauche, chicken, sagittarius, cyclone, foment)
scheme_MODE = chibi
# wasmtime wasmer lucet wax  node warpy  wace_libc
wasm_MODE = wasmtime

# Path to loccount for counting LOC stats
LOCCOUNT = loccount

# Extra options to pass to runtest.py
TEST_OPTS =

# Test with previous test files not just the test files for the
# current step. Step 0 and 1 tests are special and not included in
# later steps.
REGRESS =

HARD=
DEFERRABLE=1
OPTIONAL=1

# Run target/rule within docker image for the implementation
DOCKERIZE =


#
# Implementation specific settings
#

IMPLS = ada ada.2 awk bash basic bbc-basic c chuck clojure coffee common-lisp cpp crystal cs d dart \
	elisp elixir elm erlang es6 factor fantom forth fsharp go groovy gnu-smalltalk \
	guile haskell haxe hy io java js jq julia kotlin livescript logo lua make mal \
	matlab miniMAL nasm nim objc objpascal ocaml perl perl6 php picolisp pike plpgsql \
	plsql powershell ps python python.2 r racket rexx rpython ruby rust scala scheme skew \
	swift swift3 swift4 swift5 tcl ts vala vb vhdl vimscript wasm wren yorick xslt zig

EXTENSION = .mal

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

argv_STEP = step6_file


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

step5_EXCLUDES += bash        # never completes at 10,000
step5_EXCLUDES += basic       # too slow, and limited to ints of 2^16
step5_EXCLUDES += logo        # too slow for 10,000
step5_EXCLUDES += make        # no TCO capability (iteration or recursion)
step5_EXCLUDES += mal         # host impl dependent
step5_EXCLUDES += matlab      # never completes at 10,000
step5_EXCLUDES += plpgsql     # too slow for 10,000
step5_EXCLUDES += plsql       # too slow for 10,000
step5_EXCLUDES += powershell  # too slow for 10,000
step5_EXCLUDES += $(if $(filter cpp,$(haxe_MODE)),haxe,) # cpp finishes 10,000, segfaults at 100,000
step5_EXCLUDES += xslt		  # iteration cannot be expressed

dist_EXCLUDES += mal
# TODO: still need to implement dist
dist_EXCLUDES += guile io julia matlab swift


# Extra options to pass to runtest.py
bbc-basic_TEST_OPTS = --test-timeout 60
guile_TEST_OPTS = --test-timeout 120
io_TEST_OPTS = --test-timeout 120
logo_TEST_OPTS = --start-timeout 60 --test-timeout 120
mal_TEST_OPTS = --start-timeout 60 --test-timeout 120
miniMAL_TEST_OPTS = --start-timeout 60 --test-timeout 120
perl6_TEST_OPTS = --test-timeout=60
plpgsql_TEST_OPTS = --start-timeout 60 --test-timeout 180
plsql_TEST_OPTS = --start-timeout 120 --test-timeout 120
vimscript_TEST_OPTS = --test-timeout 30
ifeq ($(MAL_IMPL),vimscript)
mal_TEST_OPTS = --start-timeout 60 --test-timeout 180
else ifeq ($(MAL_IMPL),powershell)
mal_TEST_OPTS = --start-timeout 60 --test-timeout 180
endif
xslt_TEST_OPTS = --test-timeout 120


#
# Implementation specific utility functions
#

basic_STEP_TO_PROG_cbm          = impls/basic/$($(1)).bas
basic_STEP_TO_PROG_qbasic       = impls/basic/$($(1))

clojure_STEP_TO_PROG_clj        = impls/clojure/target/$($(1)).jar
clojure_STEP_TO_PROG_cljs       = impls/clojure/src/mal/$($(1)).cljc

haxe_STEP_TO_PROG_neko          = impls/haxe/$($(1)).n
haxe_STEP_TO_PROG_python        = impls/haxe/$($(1)).py
haxe_STEP_TO_PROG_cpp           = impls/haxe/cpp/$($(1))
haxe_STEP_TO_PROG_js            = impls/haxe/$($(1)).js

scheme_STEP_TO_PROG_chibi       = impls/scheme/$($(1)).scm
scheme_STEP_TO_PROG_kawa        = impls/scheme/out/$($(1)).class
scheme_STEP_TO_PROG_gauche      = impls/scheme/$($(1)).scm
scheme_STEP_TO_PROG_chicken     = impls/scheme/$($(1))
scheme_STEP_TO_PROG_sagittarius = impls/scheme/$($(1)).scm
scheme_STEP_TO_PROG_cyclone     = impls/scheme/$($(1))
scheme_STEP_TO_PROG_foment      = impls/scheme/$($(1)).scm

# Map of step (e.g. "step8") to executable file for that step
ada_STEP_TO_PROG =           impls/ada/$($(1))
ada.2_STEP_TO_PROG =         impls/ada.2/$($(1))
awk_STEP_TO_PROG =           impls/awk/$($(1)).awk
bash_STEP_TO_PROG =          impls/bash/$($(1)).sh
basic_STEP_TO_PROG =         $(basic_STEP_TO_PROG_$(basic_MODE))
bbc-basic_STEP_TO_PROG =     impls/bbc-basic/$($(1)).bas
c_STEP_TO_PROG =             impls/c/$($(1))
chuck_STEP_TO_PROG =         impls/chuck/$($(1)).ck
clojure_STEP_TO_PROG =       $(clojure_STEP_TO_PROG_$(clojure_MODE))
coffee_STEP_TO_PROG =        impls/coffee/$($(1)).coffee
common-lisp_STEP_TO_PROG =   impls/common-lisp/$($(1))
cpp_STEP_TO_PROG =           impls/cpp/$($(1))
crystal_STEP_TO_PROG =       impls/crystal/$($(1))
cs_STEP_TO_PROG =            impls/cs/$($(1)).exe
d_STEP_TO_PROG =             impls/d/$($(1))
dart_STEP_TO_PROG =          impls/dart/$($(1)).dart
elisp_STEP_TO_PROG =         impls/elisp/$($(1)).el
elixir_STEP_TO_PROG =        impls/elixir/lib/mix/tasks/$($(1)).ex
elm_STEP_TO_PROG =           impls/elm/$($(1)).js
erlang_STEP_TO_PROG =        impls/erlang/$($(1))
es6_STEP_TO_PROG =           impls/es6/$($(1)).mjs
factor_STEP_TO_PROG =        impls/factor/$($(1))/$($(1)).factor
fantom_STEP_TO_PROG =        impls/fantom/lib/fan/$($(1)).pod
forth_STEP_TO_PROG =         impls/forth/$($(1)).fs
fsharp_STEP_TO_PROG =        impls/fsharp/$($(1)).exe
go_STEP_TO_PROG =            impls/go/$($(1))
groovy_STEP_TO_PROG =        impls/groovy/$($(1)).groovy
gnu-smalltalk_STEP_TO_PROG = impls/gnu-smalltalk/$($(1)).st
guile_STEP_TO_PROG =         impls/guile/$($(1)).scm
haskell_STEP_TO_PROG =       impls/haskell/$($(1))
haxe_STEP_TO_PROG =          $(haxe_STEP_TO_PROG_$(haxe_MODE))
hy_STEP_TO_PROG =            impls/hy/$($(1)).hy
io_STEP_TO_PROG =            impls/io/$($(1)).io
java_STEP_TO_PROG =          impls/java/target/classes/mal/$($(1)).class
js_STEP_TO_PROG =            impls/js/$($(1)).js
jq_STEP_PROG =               impls/jq/$($(1)).jq
julia_STEP_TO_PROG =         impls/julia/$($(1)).jl
kotlin_STEP_TO_PROG =        impls/kotlin/$($(1)).jar
livescript_STEP_TO_PROG =    impls/livescript/$($(1)).js
logo_STEP_TO_PROG =          impls/logo/$($(1)).lg
lua_STEP_TO_PROG =           impls/lua/$($(1)).lua
make_STEP_TO_PROG =          impls/make/$($(1)).mk
mal_STEP_TO_PROG =           impls/mal/$($(1)).mal
matlab_STEP_TO_PROG =        impls/matlab/$($(1)).m
miniMAL_STEP_TO_PROG =       impls/miniMAL/$($(1)).json
nasm_STEP_TO_PROG =          impls/nasm/$($(1))
nim_STEP_TO_PROG =           impls/nim/$($(1))
objc_STEP_TO_PROG =          impls/objc/$($(1))
objpascal_STEP_TO_PROG =     impls/objpascal/$($(1))
ocaml_STEP_TO_PROG =         impls/ocaml/$($(1))
perl_STEP_TO_PROG =          impls/perl/$($(1)).pl
perl6_STEP_TO_PROG =         impls/perl6/$($(1)).pl
php_STEP_TO_PROG =           impls/php/$($(1)).php
picolisp_STEP_TO_PROG =      impls/picolisp/$($(1)).l
pike_STEP_TO_PROG =          impls/pike/$($(1)).pike
plpgsql_STEP_TO_PROG =       impls/plpgsql/$($(1)).sql
plsql_STEP_TO_PROG =         impls/plsql/$($(1)).sql
powershell_STEP_TO_PROG =    impls/powershell/$($(1)).ps1
ps_STEP_TO_PROG =            impls/ps/$($(1)).ps
python_STEP_TO_PROG =        impls/python/$($(1)).py
python.2_STEP_TO_PROG =      impls/python.2/$($(1)).py
r_STEP_TO_PROG =             impls/r/$($(1)).r
racket_STEP_TO_PROG =        impls/racket/$($(1)).rkt
rexx_STEP_TO_PROG =          impls/rexx/$($(1)).rexxpp
rpython_STEP_TO_PROG =       impls/rpython/$($(1))
ruby_STEP_TO_PROG =          impls/ruby/$($(1)).rb
rust_STEP_TO_PROG =          impls/rust/$($(1))
scala_STEP_TO_PROG =         impls/scala/target/scala-2.11/classes/$($(1)).class
scheme_STEP_TO_PROG =        $(scheme_STEP_TO_PROG_$(scheme_MODE))
skew_STEP_TO_PROG =          impls/skew/$($(1)).js
swift_STEP_TO_PROG =         impls/swift/$($(1))
swift3_STEP_TO_PROG =        impls/swift3/$($(1))
swift4_STEP_TO_PROG =        impls/swift4/$($(1))
swift5_STEP_TO_PROG =        impls/swift5/$($(1))
tcl_STEP_TO_PROG =           impls/tcl/$($(1)).tcl
ts_STEP_TO_PROG =            impls/ts/$($(1)).js
vala_STEP_TO_PROG =          impls/vala/$($(1))
vb_STEP_TO_PROG =            impls/vb/$($(1)).exe
vhdl_STEP_TO_PROG =          impls/vhdl/$($(1))
vimscript_STEP_TO_PROG =     impls/vimscript/$($(1)).vim
wasm_STEP_TO_PROG =          impls/wasm/$($(1)).$(if $(filter lucet,$(wasm_MODE)),so,wasm)
wren_STEP_TO_PROG =          impls/wren/$($(1)).wren
yorick_STEP_TO_PROG =        impls/yorick/$($(1)).i
xslt_STEP_TO_PROG =          impls/xslt/$($(1))
zig_STEP_TO_PROG =           impls/zig/$($(1))

#
# General settings and utility functions
#

# Needed some argument munging
COMMA = ,
noop =
SPACE = $(noop) $(noop)
export FACTOR_ROOTS := .

opt_HARD            = $(if $(strip $(HARD)),$(if $(filter t true T True TRUE 1 y yes Yes YES,$(HARD)),--hard,),)
opt_DEFERRABLE      = $(if $(strip $(DEFERRABLE)),$(if $(filter t true T True TRUE 1 y yes Yes YES,$(DEFERRABLE)),--deferrable,--no-deferrable),--no-deferrable)
opt_OPTIONAL        = $(if $(strip $(OPTIONAL)),$(if $(filter t true T True TRUE 1 y yes Yes YES,$(OPTIONAL)),--optional,--no-optional),--no-optional)

# Return list of test files for a given step. If REGRESS is set then
# test files will include step 2 tests through tests for the step
# being tested.
STEP_TEST_FILES = $(strip $(wildcard \
		    $(foreach s,$(if $(strip $(REGRESS)),\
			$(filter-out $(if $(filter $(1),$(step5_EXCLUDES)),step5,),\
			  $(regress_$(2)))\
			,$(2)),\
		      impls/$(1)/tests/$($(s))$(EXTENSION) impls/tests/$($(s))$(EXTENSION))))

# DOCKERIZE utility functions
lc = $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$1))))))))))))))))))))))))))
impl_to_image = kanaka/mal-test-$(call lc,$(1))

actual_impl = $(if $(filter mal,$(1)),$(patsubst %-mal,%,$(MAL_IMPL)),$(1))

# Takes impl
# Returns nothing if DOCKERIZE is not set, otherwise returns the
# docker prefix necessary to run make within the docker environment
# for this impl
get_build_command = $(strip $(foreach mode,$(1)_MODE, \
    $(if $(strip $(DOCKERIZE)),\
      docker run \
      -it --rm -u $(shell id -u) \
      -v $(dir $(abspath $(lastword $(MAKEFILE_LIST)))):/mal \
      -w /mal/impls/$(1) \
      $(if $(strip $($(mode))),-e $(mode)=$($(mode)),) \
      $(if $(filter factor,$(1)),-e FACTOR_ROOTS=$(FACTOR_ROOTS),) \
      $(call impl_to_image,$(1)) \
      $(MAKE) $(if $(strip $($(mode))),$(mode)=$($(mode)),) \
      ,\
      $(MAKE) $(if $(strip $($(mode))),$(mode)=$($(mode)),) -C impls/$(impl))))

# Takes impl and step args. Optional env vars and dockerize args
# Returns a command prefix (docker command and environment variables)
# necessary to launch the given impl and step
get_run_prefix = $(strip $(foreach mode,$(call actual_impl,$(1))_MODE, \
    $(if $(strip $(DOCKERIZE) $(4)),\
      docker run -e STEP=$($2) -e MAL_IMPL=$(MAL_IMPL) \
      -it --rm -u $(shell id -u) \
      -v $(dir $(abspath $(lastword $(MAKEFILE_LIST)))):/mal \
      -w /mal/impls/$(call actual_impl,$(1)) \
      $(if $(strip $($(mode))),-e $(mode)=$($(mode)),) \
      $(if $(filter factor,$(1)),-e FACTOR_ROOTS=$(FACTOR_ROOTS),) \
      $(foreach env,$(3),-e $(env)) \
      $(call impl_to_image,$(call actual_impl,$(1))) \
      ,\
      env STEP=$($2) MAL_IMPL=$(MAL_IMPL) \
      $(if $(strip $($(mode))),$(mode)=$($(mode)),) \
      $(if $(filter factor,$(1)),FACTOR_ROOTS=$(FACTOR_ROOTS),) \
      $(3))))

# Takes impl and step
# Returns the runtest command prefix (with runtest options) for testing the given step
get_runtest_cmd = $(call get_run_prefix,$(1),$(2),$(if $(filter cs fsharp mal tcl vb,$(1)),RAW=1,)) \
		    ../../runtest.py $(opt_HARD) $(opt_DEFERRABLE) $(opt_OPTIONAL) $(call $(1)_TEST_OPTS) $(TEST_OPTS)

# Takes impl and step
# Returns the runtest command prefix (with runtest options) for testing the given step
get_argvtest_cmd = $(call get_run_prefix,$(1),$(2)) ../tests/run_argv_test.sh

# Derived lists
STEPS = $(sort $(filter-out %_EXCLUDES,$(filter step%,$(.VARIABLES))))
DO_IMPLS = $(filter-out $(SKIP_IMPLS),$(IMPLS))
IMPL_TESTS = $(foreach impl,$(DO_IMPLS),test^$(impl))
STEP_TESTS = $(foreach step,$(STEPS),test^$(step))
ALL_TESTS = $(filter-out $(foreach e,$(step5_EXCLUDES),test^$(e)^step5),\
              $(strip $(sort \
                $(foreach impl,$(DO_IMPLS),\
                  $(foreach step,$(STEPS),test^$(impl)^$(step))))))
ALL_BUILDS = $(strip $(sort \
               $(foreach impl,$(DO_IMPLS),\
                 $(foreach step,$(STEPS),build^$(impl)^$(step)))))

DOCKER_BUILD = $(foreach impl,$(DO_IMPLS),docker-build^$(impl))

DOCKER_SHELL = $(foreach impl,$(DO_IMPLS),docker-shell^$(impl))

IMPL_PERF = $(foreach impl,$(filter-out $(perf_EXCLUDES),$(DO_IMPLS)),perf^$(impl))

IMPL_STATS = $(foreach impl,$(DO_IMPLS),stats^$(impl))

IMPL_REPL = $(foreach impl,$(DO_IMPLS),repl^$(impl))
ALL_REPL = $(strip $(sort \
             $(foreach impl,$(DO_IMPLS),\
               $(foreach step,$(STEPS),repl^$(impl)^$(step)))))


#
# Build rules
#

# Enable secondary expansion for all rules
.SECONDEXPANSION:

# Build a program in an implementation directory
# Make sure we always try and build first because the dependencies are
# encoded in the implementation Makefile not here
.PHONY: $(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(call $(i)_STEP_TO_PROG,$(s))))
$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(call $(i)_STEP_TO_PROG,$(s)))):
	$(foreach impl,$(word 2,$(subst /, ,$(@))),\
	  $(if $(DOCKERIZE), \
	    $(call get_build_command,$(impl)) $(patsubst impls/$(impl)/%,%,$(@)), \
	    $(call get_build_command,$(impl)) $(subst impls/$(impl)/,,$(@))))

# Allow IMPL, build^IMPL, IMPL^STEP, and build^IMPL^STEP
$(DO_IMPLS): $$(foreach s,$$(STEPS),$$(call $$(@)_STEP_TO_PROG,$$(s)))

$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),build^$(i))): $$(foreach s,$$(STEPS),$$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(s)))

$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),$(i)^$(s))): $$(call $$(word 1,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 2,$$(subst ^, ,$$(@))))

$(foreach i,$(DO_IMPLS),$(foreach s,$(STEPS),build^$(i)^$(s))): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))



#
# Test rules
#

$(ALL_TESTS): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  $(foreach step,$(word 3,$(subst ^, ,$(@))),\
	    echo "(call STEP_TEST_FILES,$(impl),$(step)): $(call STEP_TEST_FILES,$(impl),$(step))" && \
	    cd impls/$(call actual_impl,$(impl)) && \
	    $(foreach test,$(patsubst impls/%,%,$(call STEP_TEST_FILES,$(impl),$(step))),\
	      echo '----------------------------------------------' && \
	      echo 'Testing $@; step file: $+, test file: $(test)' && \
	      echo 'Running: $(call get_runtest_cmd,$(impl),$(step)) ../$(test) -- ../$(impl)/run' && \
	      $(call get_runtest_cmd,$(impl),$(step)) ../$(test) -- ../$(impl)/run && \
	      $(if $(filter tests/$(argv_STEP)$(EXTENSION),$(test)),\
	        echo '----------------------------------------------' && \
	        echo 'Testing ARGV of $@; step file: $+' && \
	        echo 'Running: $(call get_argvtest_cmd,$(impl),$(step)) ../$(impl)/run ' && \
	        $(call get_argvtest_cmd,$(impl),$(step)) ../$(impl)/run  && ,\
		true && ))\
	    true))

# Allow test, tests, test^STEP, test^IMPL, and test^IMPL^STEP
test: $(ALL_TESTS)
tests: $(ALL_TESTS)

$(IMPL_TESTS): $$(filter $$@^%,$$(ALL_TESTS))

$(STEP_TESTS): $$(foreach step,$$(subst test^,,$$@),$$(filter %^$$(step),$$(ALL_TESTS)))


#
# Docker build rules
#

docker-build: $(DOCKER_BUILD)

$(DOCKER_BUILD):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Running: docker build -t $(call impl_to_image,$(impl)) .:"; \
	  cd impls/$(impl) && docker build -t $(call impl_to_image,$(impl)) .)

#
# Docker shell rules
#

$(DOCKER_SHELL):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Running: $(call get_run_prefix,$(impl),stepA,,dockerize) bash"; \
	  $(call get_run_prefix,$(impl),stepA,,dockerize) bash)


#
# Performance test rules
#

perf: $(IMPL_PERF)

$(IMPL_PERF):
	@echo "----------------------------------------------"; \
	$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  cd impls/$(call actual_impl,$(impl)); \
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

$(ALL_REPL): $$(call $$(word 2,$$(subst ^, ,$$(@)))_STEP_TO_PROG,$$(word 3,$$(subst ^, ,$$(@))))
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  $(foreach step,$(word 3,$(subst ^, ,$(@))),\
	    cd impls/$(call actual_impl,$(impl)); \
	    echo 'REPL implementation $(impl), step file: $+'; \
	    echo 'Running: $(call get_run_prefix,$(impl),$(step)) ../$(impl)/run $(RUN_ARGS)'; \
	    $(call get_run_prefix,$(impl),$(step)) ../$(impl)/run $(RUN_ARGS);))

# Allow repl^IMPL^STEP and repl^IMPL (which starts REPL of stepA)
$(IMPL_REPL): $$@^stepA

#
# Stats test rules
#

# For a concise summary:
#   make stats | egrep -A1 "^Stats for|^all" | egrep -v "^all|^--"
stats: $(IMPL_STATS)

$(IMPL_STATS):
	@$(foreach impl,$(word 2,$(subst ^, ,$(@))),\
	  echo "Stats for $(impl):"; \
	  $(LOCCOUNT) -x "[sS]tep[0-9]_.*|[.]md$$|tests|examples|Makefile|package.json|tsconfig.json|Cargo.toml|project.clj|node_modules|getline.cs|terminal.cs|elm-stuff|objpascal/regexpr|rdyncall|swift/templates" impls/$(impl))

#
# Utility functions
#
print-%:
	@echo "$($(*))"

#
# Recursive rules (call make FOO in each subdirectory)
#

define recur_template
.PHONY: $(1)
$(1): $(2)
$(2):
	@echo "----------------------------------------------"; \
	$$(foreach impl,$$(word 2,$$(subst ^, ,$$(@))),\
	  echo "Running: $$(call get_build_command,$$(impl)) --no-print-directory $(1)"; \
	  $$(call get_build_command,$$(impl)) --no-print-directory $(1))
endef

recur_impls_ = $(filter-out $(foreach impl,$($(1)_EXCLUDES),$(1)^$(impl)),$(foreach impl,$(IMPLS),$(1)^$(impl)))

# recursive clean
$(eval $(call recur_template,clean,$(call recur_impls_,clean)))

# recursive dist
$(eval $(call recur_template,dist,$(call recur_impls_,dist)))
