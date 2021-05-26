STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

all: $(STEPS)

%: src/%.zig
	zig build -Drelease-fast=true $@

.PHONY: clean

clean:
	rm -rf $(STEPS) $(STEPS:%=%.o) {.,**}/zig-cache/
