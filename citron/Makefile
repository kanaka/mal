PROGS=step0_repl.ctr step1_read_print.ctr step2_eval.ctr step3_env.ctr step4_if_fn_do.ctr\
	  step5_tco.ctr step6_file.ctr step7_quote.ctr step8_macros.ctr step9_try.ctr stepA_mal.ctr

.PHONY: linenoise/liblinenoise.so

linenoise/liblinenoise.so:
	cd linenoise &&\
	gcc -c -fpic linenoise.c &&\
	gcc -shared linenoise.o -o liblinenoise.so

${PROGS}: linenoise/liblinenoise.so

