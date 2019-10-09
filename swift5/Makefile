ifneq ($(shell which xcrun),)
	SWIFT = xcrun -sdk macosx swiftc
else
	SWIFT = swiftc
endif

STEP3_DEPS = Sources/types.swift Sources/reader.swift Sources/printer.swift Sources/env.swift
STEP4_DEPS = $(STEP3_DEPS) Sources/core.swift

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

