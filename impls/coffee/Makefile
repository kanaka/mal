SOURCES_BASE = node_readline.coffee types.coffee \
	       reader.coffee printer.coffee
SOURCES_LISP = env.coffee core.coffee stepA_mal.coffee
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

STEPS = step0_repl.coffee step1_read_print.coffee \
	step2_eval.coffee step3_env.coffee step4_if_fn_do.coffee \
	step5_tco.coffee step6_file.coffee step7_quote.coffee \
	step8_macros.coffee step9_try.coffee stepA_mal.coffee

all: node_modules dist

node_modules:
	npm install

$(STEPS): node_modules

dist: mal.coffee mal

mal.coffee: $(SOURCES)
	cat $+ | grep -v "= *require('./" > $@

mal: mal.coffee
	echo "#!/usr/bin/env coffee" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.coffee mal

