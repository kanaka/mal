SHELL := bash

ROOT := $(shell cd ../.. && pwd)
BASE := $(shell pwd)

export PERL5LIB := $(BASE)/lib

ifndef BASIC
export DEFERRABLE := 1
export OPTIONAL := 1
endif

IMPL ?= $(shell basename $(BASE))

ALL_TESTS := $(shell echo test-{{0..9},A})

GREP_RESULTS := grep -E '(^TEST RESULTS|: .* tests$$)'


default:

test:
	time $(MAKE) test-all BASIC=$(BASIC) |& $(GREP_RESULTS)

test-all: $(ALL_TESTS)

test-self-host: docker-build
	time $(MAKE) -C $(ROOT) MAL_IMPL=$(IMPL) test^mal^step{{0..4},{6..9},A} |& \
	    $(GREP_RESULTS)

$(ALL_TESTS):
	time $(MAKE) --no-print-directory -C $(ROOT) test^$(IMPL)^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL)

docker-build:
	$(MAKE) -C $(ROOT) docker-build^$(IMPL)
	@echo
