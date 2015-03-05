all:	step0_repl


step0_repl:	step0_repl.adb
	gnatmake -f -g -o $@ $<

clean:
	rm -f b~* *.o *.ali step0_repl
