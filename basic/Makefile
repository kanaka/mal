
step%.bas: step%.in.bas
	./qb2cbm.sh $< > $@

step0_repl.bas: readline.in.bas
step1_read_print.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas
step2_eval.bas: readline.in.bas types.in.bas reader.in.bas printer.in.bas

tests/%.bas: tests/%.in.bas readline.in.bas types.in.bas reader.in.bas printer.in.bas
	./qb2cbm.sh $< > $@

