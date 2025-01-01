# List of Lean binaries you want to produce
LEAN_BINARIES = step0_repl step1_read_print step2_eval step3_env \
                step4_if_fn_do step5_tco step6_file step7_quote \
                step8_macros step9_try stepA_mal

# Default target to build all binaries
all: $(LEAN_BINARIES)

# Target for each Lean binary
$(LEAN_BINARIES): % : .lake/build/bin/%
	cp $< ./

# Trigger the Lean build if binaries do not exist
.lake/build/bin/%:
	lake build

# Phony targets to avoid file name conflicts
.PHONY: all clean

# Clean up all generated binaries
clean:
	rm -f $(LEAN_BINARIES) && rm -rf ./.lake/build/bin
