SHELL := bash

ROOT := $(shell cd ../.. && pwd)
BASE := $(shell pwd)

export PERL5LIB := $(BASE)/lib

ifndef BASIC
export DEFERRABLE := 1
export OPTIONAL := 1
endif

IMPL ?= $(shell basename $(BASE))

ALL_TESTS := $(shell echo test-{0..9})


default:

test:
	$(MAKE) test-all BASIC=$(BASIC) |& \
	    grep -E '(^TEST RESULTS|: .* tests$$)'

test-all: $(ALL_TESTS)

$(ALL_TESTS):
	$(MAKE) --no-print-directory -C $(ROOT) test^$(IMPL)^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL)
