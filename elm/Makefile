SOURCES_BASE = #reader.ls printer.ls env.ls core.ls utils.ls
SOURCES_STEPS = step0_repl.elm step1_read_print.elm step2_eval.elm \
	step3_env.elm # \
	step4_if_fn_do.ls step5_tco.ls step6_file.ls step7_quote.ls \
	step8_macros.ls step9_try.ls stepA_mal.ls
SOURCES_LISP = #env.ls core.ls stepA_mal.ls
SOURCES = $(SOURCES_BASE) $(SOURCES_STEPS)

BINS = $(SOURCES:%.elm=%.js)

ELM = node_modules/.bin/elm

all: node_modules $(BINS)

node_modules:
	npm install

elm_packages:
	$(ELM) package install

%.js: %.elm node_modules
	$(ELM) make $(@:%.js=%.elm) --output $@

step1_read_print.js: Reader.elm Printer.elm Utils.elm Types.elm
step2_eval.js: Reader.elm Printer.elm Utils.elm Types.elm
step3_env.js: Reader.elm Printer.elm Utils.elm Types.elm Env.elm
# step4_if_fn_do.js: utils.js reader.js printer.js env.js core.js
# step5_tco.js: utils.js reader.js printer.js env.js core.js
# step6_file.js: utils.js reader.js printer.js env.js core.js
# step7_quote.js: utils.js reader.js printer.js env.js core.js
# step8_macros.js: utils.js reader.js printer.js env.js core.js
# step9_try.js: utils.js reader.js printer.js env.js core.js
# stepA_mal.js: utils.js reader.js printer.js env.js core.js

clean:
	rm -f $(BINS)

# stats: $(SOURCES)
# 	@wc $^
# 	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"

# stats-lisp: $(SOURCES_LISP)
# 	@wc $^
# 	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"
