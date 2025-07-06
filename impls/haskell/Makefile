BINS4 = step4_if_fn_do step5_tco step6_file step7_quote step8_macros \
  step9_try stepA_mal
BINS3 = step3_env $(BINS4)
BINS1 = step1_read_print step2_eval $(BINS3)
BINS = step0_repl $(BINS1)
ghc_flags = -Wall -Wextra
LDLIBS = -lreadline

#####################

all: $(BINS)

$(BINS): %: %.hs
	ghc ${ghc_flags} --make $< $(LDLIBS) -o $@

$(BINS1): Types.hs Reader.hs Printer.hs
$(BINS3): Env.hs
$(BINS4): Core.hs
$(BINS): Readline.hs

clean:
	rm -f $(BINS) *.hi *.o
