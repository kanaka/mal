BASICPP_OPTS = --combine-lines

step%.bas: step%.in.bas
	./basicpp.py $(BASICPP_OPTS) $< > $@

step%.prg: step%.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	#cat $< | sed 's/["]\@<!\<\w\+\>["]\@!/\L&/g' > $<.tmp
	petcat -text -w2 -o $@ $<.tmp
	#rm $<.tmp

STEPS4_A = step4_if_fn_do.bas step5_tco.bas step6_file.bas \
           step7_quote.bas step8_macros.bas step9_try.bas stepA_mal.bas
STEPS3_A = step3_env.bas $(STEPS4_A)
STEPS1_A = step1_read_print.bas step2_eval.bas $(STEPS3_A)
STEPS0_A = step0_repl.bas $(STEPS1_A)

$(STEPS0_A): readline.in.bas
$(STEPS1_A): debug.in.bas types.in.bas reader.in.bas printer.in.bas
$(STEPS3_A): env.in.bas
$(STEPS4_A): core.in.bas

tests/%.bas: tests/%.in.bas
	./basicpp.py $(BASICPP_OPTS) $< > $@

tests/%.prg: tests/%.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	petcat -text -w2 -o $@ $<.tmp
	rm $<.tmp

mal.prg: stepA_mal.prg
	cp $< $@

SOURCES_LISP = env.in.bas core.in.bas stepA_mal.in.bas
SOURCES = readline.in.bas types.in.bas reader.in.bas printer.in.bas $(SOURCES_LISP)

.PHONY: clean stats

clean:
	rm -f $(STEPS0_A) $(subst .bas,.prg,$(STEPS0_A))

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*REM |^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*REM |^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

