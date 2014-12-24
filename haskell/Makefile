SOURCES_BASE = 
SOURCES_LISP = step0_repl.hs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.hs step1_read_print.hs step2_eval.hs step3_env.hs
OTHER_SRCS = Types.hs Reader.hs Printer.hs Env.hs
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
stats-lisp: $(SOURCES_LISP)
	@wc $^
