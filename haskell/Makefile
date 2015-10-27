SOURCES_BASE = Readline.hs Types.hs Reader.hs Printer.hs
SOURCES_LISP = Env.hs Core.hs step9_try.hs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.hs step1_read_print.hs step2_eval.hs step3_env.hs \
       step4_if_fn_do.hs step5_tco.hs step6_file.hs step7_quote.hs \
       step8_macros.hs step9_try.hs stepA_mal.hs
OTHER_SRCS = Readline.hs Types.hs Reader.hs Printer.hs Env.hs Core.hs
BINS = $(SRCS:%.hs=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.hs $(OTHER_SRCS)
	ghc --make $< -o $@

clean:
	rm -f $(BINS) mal *.hi *.o

.PHONY: stats stats-lisp tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
