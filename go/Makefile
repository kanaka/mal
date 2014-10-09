export GOPATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

#####################

SOURCES_BASE = src/types/types.go src/reader/reader.go src/printer/printer.go \
	       src/env/env.go src/core/core.go
SOURCES_LISP = src/stepA_more/stepA_more.go
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.go step1_read_print.go step2_eval.go step3_env.go \
       step4_if_fn_do.go step5_tco.go step6_file.go step7_quote.go \
       step8_macros.go stepA_more.go
BINS = $(SRCS:%.go=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

define dep_template
$(1): $(SOURCES_BASE) src/$(1)/$(1).go
	go build $$@
endef

$(foreach b,$(BINS),$(eval $(call dep_template,$(b))))

clean:
	rm -f $(BINS) mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^
