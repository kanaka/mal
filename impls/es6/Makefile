SOURCES_BASE = node_readline.js types.mjs reader.mjs printer.mjs
SOURCES_LISP = env.mjs core.mjs stepA_mal.mjs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

STEPS = step0_repl.mjs step1_read_print.mjs step2_eval.mjs step3_env.mjs \
	step4_if_fn_do.mjs step5_tco.mjs step6_file.mjs \
	step7_quote.mjs step8_macros.mjs step9_try.mjs stepA_mal.mjs

all: node_modules

dist: mal.js mal

node_modules:
	npm install

$(STEPS): node_modules

mal.js: $(SOURCES)
	cat $+ | sed 's/^export //' | grep -v "^import " >> $@

mal: mal.js
	echo "#!/usr/bin/env node" > $@
	cat $< >> $@
	chmod +x $@


clean:
	rm -f mal.js mal
	rm -rf node_modules
