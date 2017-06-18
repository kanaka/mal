SOURCES_BASE = Reader.elm Printer.elm Utils.elm Types.elm Env.elm \
	Core.elm Eval.elm IO.elm
SOURCES_STEPS = step0_repl.elm step4_if_fn_do.elm step5_tco.elm step6_file.elm step1_read_print.elm step7_quote.elm 
	#step2_eval.elm step3_env.elm \
	step8_macros.ls step9_try.ls stepA_mal.ls
SOURCES_LISP = #env.ls core.ls stepA_mal.ls
SOURCES = $(SOURCES_STEPS)

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
#step2_eval.js: Reader.elm Printer.elm Utils.elm Types.elm
#step3_env.js: Reader.elm Printer.elm Utils.elm Types.elm Env.elm
step4_if_fn_do.js: $(SOURCES_BASE)
step5_tco.js: $(SOURCES_BASE)
step6_file.js: $(SOURCES_BASE)
step7_quote.js: $(SOURCES_BASE)
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
