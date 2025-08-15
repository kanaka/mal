M := $(or $(MAKES_REPO_DIR),.cache/makes)
$(shell [ -d $M ] || git clone -q https://github.com/makeplus/makes $M)
include $M/init.mk
include $M/clean.mk
include $M/ys.mk
include $M/shell.mk

ROOT := $(shell cd ../.. && pwd)

ifndef BASIC
export DEFERRABLE := 1
export OPTIONAL := 1
endif

IMPL := yamlscript

STEPS := $(shell for f in step*; do echo $${f%%_*}; done)
STEPS := $(STEPS:step%=%)
ALL-TESTS := $(STEPS:%=test-%)

GREP-RESULTS := grep -E '(^TEST RESULTS|: .* tests$$)'

export YSPATH := lib


test:
	time $(MAKE) test-all BASIC=$(BASIC) |& $(GREP-RESULTS)
	echo

test-all: $(ALL-TESTS)

test-docker: docker-build
	$(MAKE) -C $(ROOT) DOCKERIZE=1 test^$(IMPL)

test-self-host: docker-build
	time $(MAKE) -C $(ROOT) MAL_IMPL=$(IMPL) test^mal^step{0..2} |& \
	    $(GREP-RESULTS)

$(ALL-TESTS): $(YS)
	time $(MAKE) --no-print-directory -C ../.. \
	  test^$(IMPL)^step$(@:test-%=%) \
	  MAL_IMPL=$(IMPL) \
	  DEFERRABLE=$(DEFERRABLE) \
	  OPTIONAL=$(OPTIONAL) || \
	  [[ $@ == test-6 ]]

docker-build:
	$(MAKE) -C $(ROOT) docker-build^$(IMPL)
	@echo
