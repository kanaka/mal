PROGS=step0_repl step1_read_print step2_eval

all:	${PROGS}


step0_repl:	step0_repl.adb
	gnatmake -g -P$@

step1_read_print:	step1_read_print.adb types.ad[bs] reader.ad[bs] \
			printer.ad[bs] smart_pointers.ad[bs]
	gnatmake -g -P$@

step2_eval:	step2_eval.adb types.ad[bs] reader.ad[bs]  printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] evaluation.ad[bs]
	gnatmake -g -P$@

clean:
	rm -f obj/* ${PROGS}
