BINS = step0_repl.js step1_read_print.js step2_eval.js \
	step3_env.js step4_if_fn_do.js step5_tco.js step6_file.js \
	step7_quote.js step8_macros.js step9_try.js stepA_mal.js

all: node_modules $(BINS)

node_modules:
	npm install

STEP0_SOURCES = node_modules src/IO.elm
STEP1_SOURCES = $(STEP0_SOURCES) src/Reader.elm src/Printer.elm src/Utils.elm src/Types.elm src/Env.elm
STEP2_SOURCES = $(STEP1_SOURCES)
STEP3_SOURCES = $(STEP2_SOURCES)
STEP4_SOURCES = $(STEP3_SOURCES) src/Core.elm src/Eval.elm

step0_repl.js: src/Step0_repl.elm $(STEP0_SOURCES)
step1_read_print.js: src/Step1_read_print.elm $(STEP1_SOURCES)
step2_eval.js: src/Step2_eval.elm $(STEP2_SOURCES)
step3_env.js: src/Step3_env.elm $(STEP3_SOURCES)
step4_if_fn_do.js: src/Step4_if_fn_do.elm $(STEP4_SOURCES)
step5_tco.js: src/Step5_tco.elm $(STEP4_SOURCES)
step6_file.js: src/Step6_file.elm $(STEP4_SOURCES)
step7_quote.js: src/Step7_quote.elm $(STEP4_SOURCES)
step8_macros.js: src/Step8_macros.elm $(STEP4_SOURCES)
step9_try.js: src/Step9_try.elm $(STEP4_SOURCES)
stepA_mal.js: src/StepA_mal.elm $(STEP4_SOURCES)

$(BINS):
	elm make $< --output $@

clean:
	rm -f $(BINS)
