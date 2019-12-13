
STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros

all: $(STEPS)

dist: mal


%: %.zig
	nzig2 build #$*.zig
	ln -sf zig-cache/bin/$* .
