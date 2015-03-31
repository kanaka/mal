#####################

SOURCES_BASE = src/step0_repl.erl src/step1_read_print.erl src/step2_eval.erl src/step3_env.erl \
	src/step4_if_fn_do.erl src/step5_tco.erl src/step6_file.erl src/step7_quote.erl \
	src/step8_macros.erl src/step9_try.erl src/stepA_mal.erl
SOURCES_LISP = src/atom.erl src/core.erl src/env.erl src/printer.erl src/reader.erl src/types.erl
SOURCES = $(SOURCES_BASE) $(word $(words $(SOURCES_LISP)),${SOURCES_LISP})

#####################

SRCS = step0_repl.erl step1_read_print.erl step2_eval.erl step3_env.erl step4_if_fn_do.erl \
	step5_tco.erl step6_file.erl step7_quote.erl step8_macros.erl step9_try.erl stepA_mal.erl
BINS = $(SRCS:%.erl=%)

#####################

.PHONY: all mal clean stats stats-lisp

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

define dep_template
.PHONY: $(1)
$(1): $(SOURCES_BASE) src/$(1).erl
	MAL_STEP=$(1) rebar compile escriptize
endef

$(foreach b,$(BINS),$(eval $(call dep_template,$(b))))

clean:
	rebar clean

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^
