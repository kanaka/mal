export PATH := $(PATH):node_modules/.bin/

BABEL_OPTS = --source-maps true \
             --plugins transform-es2015-modules-commonjs

SOURCES_BASE = node_readline.js types.js reader.js printer.js
SOURCES_LISP = env.js core.js stepA_mal.js
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

STEPS = step0_repl step1_read_print step2_eval step3_env \
	step4_if_fn_do step5_tco step6_file step7_quote \
	step8_macros step9_try stepA_mal

all: node_modules $(foreach s,$(STEPS),build/$(s).js)

dist: mal.js mal

build/%.js: %.js node_modules
	@mkdir -p $(dir $@)
	babel $(BABEL_OPTS) $< --out-file $@
	@echo >> $@ # workaround node-uglifier bug

mal.js: $(foreach s,$(SOURCES),build/$(s))
	node -e 'nu = new (require("node-uglifier"))("./build/stepA_mal.js"); nu.merge().exportToFile("$@")'

mal: mal.js
	echo "#!/usr/bin/env node" > $@
	cat $< >> $@
	chmod +x $@

STEP0_DEPS = build/node_readline.js
STEP1_DEPS = $(STEP0_DEPS) build/types.js build/reader.js build/printer.js
STEP3_DEPS = $(STEP1_DEPS) build/env.js
STEP4_DEPS = $(STEP3_DEPS) build/core.js

build/step0_repl.js: $(STEP0_DEPS)
build/step1_read_print.js: $(STEP1_DEPS)
build/step2_eval.js: $(STEP1_DEPS)
build/step3_env.js: $(STEP3_DEPS)
build/step4_if_fn_do.js: $(STEP4_DEPS)
build/step5_tco.js: $(STEP4_DEPS)
build/step6_file.js: $(STEP4_DEPS)
build/step7_quote.js: $(STEP4_DEPS)
build/step8_macros.js: $(STEP4_DEPS)
build/step9_try.js: $(STEP4_DEPS)
build/stepA_mal.js: $(STEP4_DEPS)


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
