SOURCES = src/Step0_repl.elm src/Step1_read_print.elm src/Step2_eval.elm \
	  src/Step3_env.elm src/Step4_if_fn_do.elm src/Step5_tco.elm src/Step6_file.elm \
	  src/Step7_quote.elm src/Step8_macros.elm src/Step9_try.elm src/StepA_mal.elm

BINS = $(SOURCES:src/Step%.elm=step%.js)

ELM = node_modules/.bin/elm

all: node_modules $(BINS)

node_modules:
	npm install

step%.js: src/Step%.elm node_modules
	$(ELM) make $< --output $@

STEP0_SOURCES = src/IO.elm
STEP1_SOURCES = $(STEP0_SOURCES) src/Reader.elm src/Printer.elm src/Utils.elm src/Types.elm src/Env.elm
STEP2_SOURCES = $(STEP1_SOURCES)
STEP3_SOURCES = $(STEP2_SOURCES)
STEP4_SOURCES = $(STEP3_SOURCES) src/Core.elm src/Eval.elm

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
