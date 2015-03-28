all:	step0_repl step1_read_print


step0_repl:	step0_repl.adb
	gnatmake -g -P$@

step1_read_print:	step1_read_print.adb types.ad[bs] reader.ad[bs] \
			printer.ad[bs] smart_pointers.ad[bs]
	gnatmake -g -P$@

clean:
	rm -f obj/* step0_repl step1_read_print
