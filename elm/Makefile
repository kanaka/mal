SOURCES = step0_repl.elm step1_read_print.elm step2_eval.elm \
	step3_env.elm step4_if_fn_do.elm step5_tco.elm step6_file.elm \
	step7_quote.elm step8_macros.elm step9_try.elm stepA_mal.elm

BINS = $(SOURCES:%.elm=%.js)

ELM = node_modules/.bin/elm

all: node_modules $(BINS)

node_modules:
	npm install

elm_packages:
	$(ELM) package install

%.js: %.elm node_modules
	$(ELM) make $(@:%.js=%.elm) --output $@

STEP0_SOURCES = IO.elm
STEP1_SOURCES = $(STEP0_SOURCES) Reader.elm Printer.elm Utils.elm Types.elm Env.elm
STEP2_SOURCES = $(STEP1_SOURCES)
STEP3_SOURCES = $(STEP2_SOURCES)
STEP4_SOURCES = $(STEP3_SOURCES) Core.elm Eval.elm

step0_repl.js: $(STEP0_SOURCES)
step1_read_print.js: $(STEP1_SOURCES)
step2_eval.js: $(STEP2_SOURCES)
step3_env.js: $(STEP3_SOURCES)
step4_if_fn_do.js: $(STEP4_SOURCES)
step5_tco.js: $(STEP4_SOURCES)
step6_file.js: $(STEP4_SOURCES)
step7_quote.js: $(STEP4_SOURCES)
step8_macros.js: $(STEP4_SOURCES)
step9_try.js: $(STEP4_SOURCES)
stepA_mal.js: $(STEP4_SOURCES)

clean:
	rm -f $(BINS)

# stats: $(SOURCES)
# 	@wc $^
# 	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"

# stats-lisp: $(SOURCES_LISP)
# 	@wc $^
# 	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"
