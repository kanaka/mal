SOURCES = Step0_repl.elm Step1_read_print.elm Step2_eval.elm \
	Step3_env.elm Step4_if_fn_do.elm Step5_tco.elm Step6_file.elm \
	Step7_quote.elm Step8_macros.elm Step9_try.elm StepA_mal.elm

BINS = $(SOURCES:S%.elm=s%.js)

ELM_MAKE = node_modules/.bin/elm make

all: node_modules $(BINS)

node_modules:
	npm install

s%.js: S%.elm node_modules
	$(ELM_MAKE) $< --output $@

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
