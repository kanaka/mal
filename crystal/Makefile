STEPS = step0_repl.cr step1_read_print.cr step2_eval.cr step3_env.cr \
        step4_if_fn_do.cr step5_tco.cr step6_file.cr step7_quote.cr \
	step8_macros.cr step9_try.cr stepA_mal.cr

STEP1_DEPS = $(STEP0_DEPS) reader.cr printer.cr
STEP2_DEPS = $(STEP1_DEPS) types.cr
STEP3_DEPS = $(STEP2_DEPS) env.cr
STEP4_DEPS = $(STEP3_DEPS) core.cr error.cr

STEP_BINS = $(STEPS:%.cr=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS)

dist: mal

mal: $(LAST_STEP_BIN)
	cp $< $@

$(STEP_BINS): %: %.cr
	crystal build --release $<

step0_repl: $(STEP0_DEPS)
step1_read_print: $(STEP1_DEPS)
step2_eval: $(STEP2_DEPS)
step3_env: $(STEP3_DEPS)
step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal: $(STEP4_DEPS)

clean:
	rm -rf $(STEP_BINS) mal .crystal

.PHONY: all clean

