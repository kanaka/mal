
RPYTHON = rpython

UPPER_STEPS = step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal
STEPS = step0_repl step1_read_print step2_eval step3_env $(UPPER_STEPS)

all: $(STEPS)

dist: mal

mal: stepA_mal
	cp $< $@

%: %.py
	$(RPYTHON) --output=$@ $<

STEP0_DEPS = mal_readline.py
STEP1_DEPS = $(STEP0_DEPS) mal_types.py reader.py printer.py
STEP3_DEPS = $(STEP1_DEPS) env.py
STEP4_DEPS = $(STEP3_DEPS) core.py

step0_repl: $(STEP0_DEPS)
step1_read_print step2_eval: $(STEP1_DEPS)
step3_env: $(STEP3_DEPS)
$(UPPER_STEPS): $(STEP4_DEPS)

.PHONY: clean

clean:
	rm -f mal $(STEPS) *.pyc
	rm -rf __pycache__

