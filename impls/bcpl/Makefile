all: step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
     step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

%: %.b malhdr.h
	cintsys -c "bcpl $< to $@"

.DELETE_ON_ERROR:

step0_repl: readline.b types.b
step1_read_print step2_eval: printer.b reader.b readline.b types.b
step3_env: env.b printer.b reader.b readline.b types.b
step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try \
  stepA_mal: \
  core.b env.b printer.b reader.b readline.b types.b

clean:
	rm -f step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
	      step5_tco step6_file step7_quote step8_macros step9_try stepA_mal
