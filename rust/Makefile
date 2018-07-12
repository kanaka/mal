
UPPER_STEPS = step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal
STEPS = step0_repl step1_read_print step2_eval step3_env $(UPPER_STEPS)

SOURCES_BASE = types.rs reader.rs printer.rs
SOURCES_LISP = env.rs core.rs stepA_mal.rs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: $(STEPS)

dist: mal

mal: stepA_mal
	cp $< $@

%: %.rs
	cargo build --release --bin $*
	cp target/release/$* $@

STEP0_DEPS = readline.rs
STEP1_DEPS = $(STEP0_DEPS) types.rs reader.rs printer.rs
STEP3_DEPS = $(STEP1_DEPS) env.rs
STEP4_DEPS = $(STEP3_DEPS) core.rs

step0_repl: $(STEP0_DEPS)
step1_read_print step2_eval: $(STEP1_DEPS)
step3_env: $(STEP3_DEPS)
$(UPPER_STEPS): $(STEP4_DEPS)

.PHONY: clean stats stats-lisp

clean:
	cargo clean
	rm -f $(STEPS)
	rm -f mal

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

