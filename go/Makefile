export GOPATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

#####################

SOURCES_BASE = src/types/types.go src/reader/reader.go src/printer/printer.go \
	       src/env/env.go src/core/core.go
SOURCES_LISP = src/step4_if_fn_do/step4_if_fn_do.go
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.go step1_read_print.go step2_eval.go step3_env.go step4_if_fn_do.go
BINS = $(SRCS:%.go=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): $(SOURCES_BASE) $(SOURCES_LISP)
	go build $@

clean:
	rm -f $(BINS) mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^
