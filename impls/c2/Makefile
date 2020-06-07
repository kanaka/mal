step0_repl: step0_repl.c
	cc -std=c99 -g -Wall step0_repl.c -ledit -o step0_repl

reader_test: reader_test.c reader.c reader.h types.h ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h
	cc -std=c99 -g -Wall reader_test.c reader.c ./libs/linked_list/linked_list.c -ledit -lgc -o reader_test

linked_list_test: ./libs/linked_list/linked_list_test.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h
	cc -std=c99 -g -Wall ./libs/linked_list/linked_list_test.c ./libs/linked_list/linked_list.c -lgc -o linked_list_test

hashmap_test: ./libs/hashmap/hashmap_test.c ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h
	cc -std=c99 -g -Wall ./libs/linked_list/linked_list.c ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap_test.c -lgc -o hashmap_test

step1_read_print: step1_read_print.c reader.c reader.h types.h types.c  printer.c printer.h ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h
	cc -std=c99 -g -Wall step1_read_print.c reader.c types.c printer.c ./libs/linked_list/linked_list.c -ledit -lgc -o step1_read_print

step2_eval: step2_eval.c reader.c reader.h types.h types.c printer.c printer.h ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step2_eval.c reader.c printer.c types.c env.c ./libs/linked_list/linked_list.c ./libs/hashmap/hashmap.c -ledit -lgc -o step2_eval

step3_env: step3_env.c reader.c reader.h types.h printer.c printer.h env.h env.c types.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step3_env.c reader.c printer.c env.c types.c ./libs/hashmap/hashmap.c ./libs/linked_list/linked_list.c -ledit -lgc -o step3_env

step4_if_fn_do: step4_if_fn_do.c reader.c reader.h types.h printer.c printer.h env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step4_if_fn_do.c reader.c env.c types.c core.c printer.c ./libs/linked_list/linked_list.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o step4_if_fn_do

step5_tco: step5_tco.c reader.c reader.h types.h printer.c printer.h env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step5_tco.c reader.c ./libs/linked_list/linked_list.c printer.c env.c types.c core.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o step5_tco

step6_file: step6_file.c reader.c reader.h types.h printer.c printer.h env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step6_file.c reader.c ./libs/linked_list/linked_list.c printer.c env.c types.c core.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o step6_file

step7_quote: step7_quote.c reader.c reader.h types.h printer.c printer.h  env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step7_quote.c reader.c ./libs/linked_list/linked_list.c printer.c env.c types.c core.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o step7_quote

step8_macros: step8_macros.c reader.c reader.h types.h printer.c printer.h  env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step8_macros.c reader.c ./libs/linked_list/linked_list.c printer.c env.c types.c core.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o step8_macros

step9_try: step9_try.c reader.c reader.h types.h printer.c printer.h  env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall step9_try.c reader.c ./libs/linked_list/linked_list.c printer.c env.c types.c core.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o step9_try

stepA_mal: stepA_mal.c reader.c reader.h types.h printer.c printer.h  env.h env.c types.c core.h core.c ./libs/linked_list/linked_list.c ./libs/linked_list/linked_list.h ./libs/hashmap/hashmap.c ./libs/hashmap/hashmap.h
	cc -std=c99 -g -Wall stepA_mal.c reader.c ./libs/linked_list/linked_list.c printer.c env.c types.c core.c ./libs/hashmap/hashmap.c -ledit -lgc -ldl -lffi -o stepA_mal


.PHONY clean:
	rm -f reader_test linked_list_test hashmap_test step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal
