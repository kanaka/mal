SOURCES_BASE = node_readline.coffee types.coffee \
	       reader.coffee printer.coffee
SOURCES_LISP = env.coffee core.coffee stepA_mal.coffee
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: node_modules dist

node_modules:
	npm install

dist: mal.coffee mal

mal.coffee: $(SOURCES)
	cat $+ | grep -v "= *require('./" > $@

mal: mal.coffee
	echo "#!/usr/bin/env coffee" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.coffee mal

