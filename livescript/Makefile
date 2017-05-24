SOURCES_BASE = reader.ls printer.ls env.ls core.ls utils.ls
SOURCES_STEPS = step0_repl.ls step1_read_print.ls step2_eval.ls \
	step4_if_fn_do.ls step5_tco.ls step6_file.ls step7_quote.ls \
	step8_macros.ls step9_try.ls stepA_mal.ls
SOURCES = $(SOURCES_BASE) $(SOURCES_STEPS)

BINS = $(SOURCES:%.ls=%.js)

LSC = node_modules/.bin/lsc

all: node_modules $(BINS)

node_modules:
	npm install

%.js: %.ls
	$(LSC) -d -c $(@:%.js=%.ls)

clean:
	rm -f $(BINS)
