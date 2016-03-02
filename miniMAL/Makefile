
SOURCES_BASE = node_readline.js miniMAL-core.json \
	       types.json reader.json printer.json
SOURCES_LISP = env.json core.json stepA_mal.json
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: node_modules

node_modules:
	npm install

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

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
