MODE ?= libc

STEP0_DEPS = platform_$(MODE).wam string.wam printf.wam
STEP1_DEPS = $(STEP0_DEPS) types.wam mem.wam debug.wam reader.wam printer.wam
STEP3_DEPS = $(STEP1_DEPS) env.wam
STEP4_DEPS = $(STEP3_DEPS) core.wam

STEPS = step0_repl step1_read_print step2_eval step3_env \
        step4_if_fn_do step5_tco step6_file step7_quote \
        step8_macros step9_try stepA_mal

all: $(foreach s,$(STEPS),$(s).wasm)

%.wasm: %.wam
	wamp $^ > $*.wast
	wasm-as $*.wast -o $@

step0_repl.wasm: $(STEP0_DEPS)
step1_read_print.wasm step2_eval.wasm: $(STEP1_DEPS)
step3_env.wasm: $(STEP3_DEPS)
step4_if_fn_do.wasm step5_tco.wasm step6_file.wasm: $(STEP4_DEPS)
step7_quote.wasm step8_macros.wasm step9_try.wasm stepA_mal.wasm: $(STEP4_DEPS)

.PHONY: clean

clean:
	rm -f *.wast *.wasm

.PHONY: stats tests

stats: $(STEP4_DEPS) stepA_mal.wam
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(filter-out $(STEP1_DEPS),$(STEP4_DEPS)) stepA_mal.wam
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

