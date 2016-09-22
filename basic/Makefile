export KEEP_REM=0

step%.bas: step%.in.bas
	./qb2cbm.sh $< > $@

step%.prg: step%.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	#cat $< | sed 's/["]\@<!\<\w\+\>["]\@!/\L&/g' > $<.tmp
	petcat -text -w2 -o $@ $<.tmp
	#rm $<.tmp

STEP0_DEPS = readline.in.bas
STEP1_DEPS = $(STEP0_DEPS) debug.in.bas types.in.bas reader.in.bas printer.in.bas
STEP3_DEPS = $(STEP1_DEPS) env.in.bas
STEP4_DEPS = $(STEP3_DEPS) core.in.bas

step0_repl.bas: $(STEP0_DEPS)
step1_read_print.bas: $(STEP1_DEPS)
step2_eval.bas: $(STEP1_DEPS)
step3_env.bas: $(STEP3_DEPS)
step4_if_fn_do.bas: $(STEP4_DEPS)
step5_tco.bas: $(STEP4_DEPS)
step6_file.bas: $(STEP4_DEPS)
step7_quote.bas: $(STEP4_DEPS)

tests/%.bas: tests/%.in.bas
	./qb2cbm.sh $< > $@

tests/%.prg: tests/%.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	petcat -text -w2 -o $@ $<.tmp
	rm $<.tmp

mal.prg: step7_quote.prg
	cp $< $@

SOURCES_LISP = env.in.bas core.in.bas step7_quote.in.bas
SOURCES = readline.in.bas types.in.bas reader.in.bas printer.in.bas $(SOURCES_LISP)

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*REM |^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*REM |^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

