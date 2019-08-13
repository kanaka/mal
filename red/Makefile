STEPS = step0_rep.redl step1_read_print.red step2_eval.red step3_env.red step4_if_fn_do.red \
		step5_tco.red step6_file.red step7_quote.red step8_macros.red step9_try.red stepA_mal.red

all: $(patsubst %.red,%,$(STEPS))

$(STEPS):
	true

clean:
	rm -f mal