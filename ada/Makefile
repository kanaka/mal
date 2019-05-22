PROGS=step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
      step5_tco step6_file step7_quote step8_macros step9_try

all:	${PROGS} stepA_mal

obj:
	mkdir -p $@

# stepA_mal is awkward because GNAT requires the filename to be lowercase
${PROGS} stepa_mal:	force obj
	gnatmake -O3 -gnata $@.adb -D obj

# so we make stepa_mal and just move it.
stepA_mal:	stepa_mal
	mv $< $@

clean:
	rm -f ${PROGS}
	rm -rf obj

.PHONY: force

force:
