STEP0_DEPS = util.wam
STEP1_DEPS = $(STEP0_DEPS) types.wam mem.wam debug.wam reader.wam printer.wam
STEP2_DEPS = $(STEP1_DEPS)
STEP3_DEPS = $(STEP2_DEPS) env.wam

STEPS = step0_repl step1_read_print step2_eval step3_env \
        step4_if_fn_do step5_tco step6_file step7_quote \
        step8_macros step9_try stepA_mal

all: $(foreach s,$(STEPS),$(s).wasm)

%.wasm:
	wamp $^ > $*.wast
	wasm-as $*.wast -o $@

step0_repl.wasm: $(STEP0_DEPS) step0_repl.wam
step1_read_print.wasm: $(STEP1_DEPS) step1_read_print.wam
step2_eval.wasm: $(STEP2_DEPS) step2_eval.wam
step3_env.wasm: $(STEP3_DEPS) step3_env.wam

.PHONY: clean

clean:
	rm -f *.wast *.wasm
