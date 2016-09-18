
step%.bas: step%.in.bas
	./qb2cbm.sh $< > $@

step%.prg: step%.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	#cat $< | sed 's/["]\@<!\<\w\+\>["]\@!/\L&/g' > $<.tmp
	petcat -text -w2 -o $@ $<.tmp
	#rm $<.tmp

step0_repl.bas: readline.in.bas
step1_read_print.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas
step2_eval.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas
step3_env.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas env.in.bas
step4_if_fn_do.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas env.in.bas core.in.bas
step5_tco.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas env.in.bas core.in.bas

tests/%.bas: tests/%.in.bas
	./qb2cbm.sh $< > $@

tests/%.prg: tests/%.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	petcat -text -w2 -o $@ $<.tmp
	rm $<.tmp


SOURCES_LISP = env.in.bas core.in.bas step5_tco.in.bas
SOURCES = readline.in.bas types.in.bas reader.in.bas printer.in.bas $(SOURCES_LISP)

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*REM |^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*REM |^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

