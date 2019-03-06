ifneq ($(shell which xcrun),)
	SWIFT = xcrun -sdk macosx swiftc
else
	SWIFT = swiftc
endif

STEP3_DEPS = Sources/types.swift Sources/reader.swift Sources/printer.swift Sources/env.swift
STEP4_DEPS = $(STEP3_DEPS) Sources/core.swift

SOURCES = $(STEP4_DEPS) Sources/stepA_mal/main.swift
SOURCES_LISP = Sources/env.swift Sources/core.swift Sources/stepA_mal/main.swift

STEPS = step0_repl step1_read_print step2_eval step3_env \
	step4_if_fn_do step5_tco step6_file step7_quote \
	step8_macros step9_try stepA_mal

all: $(STEPS)

dist: mal

mal: stepA_mal
	cp $< $@

step1_read_print step2_eval step3_env: $(STEP3_DEPS)
step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal: $(STEP4_DEPS)

step%: Sources/step%/main.swift
	$(SWIFT) $+ -o $@

clean:
	rm -f $(STEPS) mal

.PHONY: stats tests

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
