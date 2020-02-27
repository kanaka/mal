
SOURCES_BASE = node_readline.js miniMAL-core.json \
	       types.json reader.json printer.json
SOURCES_LISP = env.json core.json stepA_mal.json
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)
FFI_STEPS = step4_if_fn_do step5_tco step6_file \
	    step7_quote step8_macros step9_try stepA_mal

all: node_modules

node_modules:
	npm install

$(foreach S,$(FFI_STEPS),$(S).json): node_modules

dist: mal.json mal

mal.json: $(filter-out %.js,$(SOURCES))
	echo '["do",' >> $@
	$(foreach f,$+,\
	  cat $(f) | egrep -v '^ *[[]"load-file"' >> $@; \
	  echo "," >> $@;)
	echo 'null]' >> $@

mal: mal.json
	echo '#!/usr/bin/env miniMAL' > $@
	cat $< >> $@
	chmod +x $@

clean:
