#####################

SOURCES_BASE = src/step0_repl.erl src/step1_read_print.erl src/step2_eval.erl
SOURCES_LISP = src/core.erl src/env.erl src/reader.erl
SOURCES = $(SOURCES_BASE) $(word $(words $(SOURCES_LISP)),${SOURCES_LISP})

#####################

SRCS = step0_repl.erl step1_read_print.erl step2_eval.erl
BINS = $(SRCS:%.erl=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

define dep_template
$(1): $(SOURCES_BASE) src/$(1).erl
	MAL_STEP=$(1) rebar compile escriptize
endef

$(foreach b,$(BINS),$(eval $(call dep_template,$(b))))

clean:
	rm -f $(BINS) mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^
