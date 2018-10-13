STEP0_DEPS = util.wast
STEP1_DEPS = $(STEP0_DEPS) types.wast mem.wast debug.wast reader.wast printer.wast
STEP2_DEPS = $(STEP1_DEPS)
STEP3_DEPS = $(STEP2_DEPS) env.wast

STEPS = step0_repl step1_read_print step2_eval step3_env \
        step4_if_fn_do step5_tco step6_file step7_quote \
        step8_macros step9_try stepA_mal

all: $(foreach s,$(STEPS),$(s).wasm)

%.wasm:
	./wastpp.py $^ > $*.wat
	wasm-as $*.wat -o $@

step0_repl.wasm: $(STEP0_DEPS) step0_repl.wast
step1_read_print.wasm: $(STEP1_DEPS) step1_read_print.wast
step2_eval.wasm: $(STEP2_DEPS) step2_eval.wast
step3_env.wasm: $(STEP3_DEPS) step3_env.wast

.PHONY: clean

clean:
	rm -f *.wat *.wasm
