# This Makefile cannot build steps separately.

STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

all: $(STEPS)

%: %.zig
# safe until the segfault is fixed.
	zig build -Drelease-safe=true

.PHONY: clean

clean:
	rm -fr zig-cache/ zig-out/
