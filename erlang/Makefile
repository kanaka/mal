#####################

SOURCES_BASE = src/atom.erl src/printer.erl src/reader.erl
SOURCES_LISP = src/core.erl src/env.erl src/types.erl src/stepA_mal.erl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

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
$(1): src/$(1).erl
	MAL_STEP=$(1) rebar compile escriptize
endef

$(foreach b,$(BINS),$(eval $(call dep_template,$(b))))

clean:
	rebar clean

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*%|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*%|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
