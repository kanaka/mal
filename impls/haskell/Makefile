SRCS = step0_repl.hs step1_read_print.hs step2_eval.hs step3_env.hs \
       step4_if_fn_do.hs step5_tco.hs step6_file.hs step7_quote.hs \
       step8_macros.hs step9_try.hs stepA_mal.hs
OTHER_SRCS = Readline.hs Types.hs Reader.hs Printer.hs Env.hs Core.hs
BINS = $(SRCS:%.hs=%)
ghc_flags = -Wall

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.hs $(OTHER_SRCS)
	ghc ${ghc_flags} --make $< -o $@

clean:
	rm -f $(BINS) mal *.hi *.o
