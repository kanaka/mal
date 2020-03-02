STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
        step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

SOURCES_BASE = util.sk types.sk reader.sk printer.sk

STEP3_DEPS = $(SOURCES_BASE) env.sk
STEP4_DEPS = $(STEP3_DEPS) core.sk

all: $(foreach s,$(STEPS),$(s).js) dist

dist: mal

step0_repl.js step1_read_print.js step2_eval.js step3_env.js: $(STEP3_DEPS)
step4_if_fn_do.js step5_tco.js step6_file.js step7_quote.js step8_macros.js step9_try.js stepA_mal.js: $(STEP4_DEPS)

%.js: %.sk
	skewc --target=js --release --output-file=$@ $^

mal: stepA_mal.js
	echo "#!/usr/bin/env node" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -rf step*.js mal

.PHONY: all dist clean
