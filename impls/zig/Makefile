
STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

# TODO: it would be nice to be able to rebuild only one step.

# safe until the segfault is fixed.
# make HARD=1 REGRESS=1 test^zig
# was absent from 1c76e857f8f40c46a9b08274a26a0cca7fb20c77 with zig/0.5.0

all $(STEPS):
	zig build -Drelease-safe=true

.PHONY: clean

clean:
	rm -fr zig-cache zig-out
