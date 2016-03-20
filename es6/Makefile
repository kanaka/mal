
SOURCES_BASE = node_readline.js types.js reader.js printer.js
SOURCES_LISP = env.js core.js stepA_mal.js
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

STEPS = step0_repl step1_read_print step2_eval step3_env \
	step4_if_fn_do step5_tco step6_file step7_quote \
	step8_macros step9_try stepA_mal

all: node_modules $(foreach s,$(STEPS),build/$(s).js)

dist: mal.js mal

build/%.js: %.js
	@mkdir -p $(dir $@)
	babel --source-maps true $< --out-file $@
	@echo >> $@ # workaround node-uglifier bug

mal.js: $(foreach s,$(SOURCES),build/$(s))
	node -e 'nu = new (require("node-uglifier"))("./build/stepA_mal.js"); nu.merge().exportToFile("$@")'

mal: mal.js
	echo "#!/usr/bin/env node" > $@
	cat $< >> $@
	chmod +x $@

build/step0_repl.js: step0_repl.js build/node_readline.js
build/step1_read_print.js: step1_read_print.js build/node_readline.js build/types.js build/reader.js build/printer.js
build/step2_eval.js: step2_eval.js build/node_readline.js build/types.js build/reader.js build/printer.js
build/step3_env.js: step3_env.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js
build/step4_if_fn_do.js: step4_if_fn_do.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js
build/step5_tco.js: step5_tco.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js
build/step6_file.js: step6_file.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js
build/step7_quote.js: step7_quote.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js
build/step8_macros.js: step8_macros.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js
build/step9_try.js: step9_try.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js
build/stepA_mal.js: stepA_mal.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js build/core.js


node_modules:
	npm install

clean:
	rm -f build/* mal.js mal

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	node $@ || exit 1; \
