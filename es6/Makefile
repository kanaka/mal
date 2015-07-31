
SOURCES = step0_repl.js step1_read_print.js step2_eval.js step3_env.js

all: $(foreach s,$(SOURCES),build/$(s))

build/%.js: %.js
	babel --source-maps true $< --out-file $@

build/step0_repl.js: step0_repl.js build/node_readline.js

build/step1_read_print.js: step1_read_print.js build/node_readline.js build/types.js build/reader.js build/printer.js

build/step2_eval.js: step2_eval.js build/node_readline.js build/types.js build/reader.js build/printer.js

build/step3_env.js: step3_env.js build/node_readline.js build/types.js build/reader.js build/printer.js build/env.js

clean:
	rm -f build/*
