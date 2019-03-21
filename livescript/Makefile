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

%.js: %.ls node_modules
	$(LSC) -d -c $(@:%.js=%.ls)

step1_read_print.js: utils.js reader.js printer.js
step2_eval.js: utils.js reader.js printer.js
step3_env.js: utils.js reader.js printer.js env.js
step4_if_fn_do.js: utils.js reader.js printer.js env.js core.js
step5_tco.js: utils.js reader.js printer.js env.js core.js
step6_file.js: utils.js reader.js printer.js env.js core.js
step7_quote.js: utils.js reader.js printer.js env.js core.js
step8_macros.js: utils.js reader.js printer.js env.js core.js
step9_try.js: utils.js reader.js printer.js env.js core.js
stepA_mal.js: utils.js reader.js printer.js env.js core.js

clean:
	rm -f $(BINS)
