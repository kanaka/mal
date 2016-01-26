# Python step rules
s%.py: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -python $@

STEP1_DEPS = types/Types.hx reader/Reader.hx printer/Printer.hx
STEP3_DEPS = $(STEP1_DEPS) env/Env.hx
STEP4_DEPS = $(STEP3_DEPS) core/Core.hx

step1_read_print.py: $(STEP1_DEPS)
step2_eval.py: $(STEP1_DEPS)
step3_env.py: $(STEP3_DEPS)
step4_if_fn_do.py: $(STEP4_DEPS)
step5_tco.py: $(STEP4_DEPS)
step6_file.py: $(STEP4_DEPS)
step7_quote.py: $(STEP4_DEPS)
step8_macros.py: $(STEP4_DEPS)
step9_try.py: $(STEP4_DEPS)
stepA_mal.py: $(STEP4_DEPS)

clean:
	rm -r *.py
