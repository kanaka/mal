STEP1_DEPS = Compat.hx types/Types.hx reader/Reader.hx printer/Printer.hx
STEP3_DEPS = $(STEP1_DEPS) env/Env.hx
STEP4_DEPS = $(STEP3_DEPS) core/Core.hx


# Neko target (neko)

s%.n: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -neko $@

step1_read_print.n step2_eval.n: $(STEP1_DEPS)
step3_env.n: $(STEP3_DEPS)
step4_if_fn_do.n step5_tco.n step6_file.n step7_quote.n step8_macros.n step9_try.n stepA_mal.n: $(STEP4_DEPS)


# Python 3 target (python)

s%.py: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -python $@

step1_read_print.py step2_eval.py: $(STEP1_DEPS)
step3_env.py: $(STEP3_DEPS)
step4_if_fn_do.py step5_tco.py step6_file.py step7_quote.py step8_macros.py step9_try.py stepA_mal.py: $(STEP4_DEPS)


# C++ target (cpp)

cpp/s%: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -cpp cpp
	cp $(patsubst cpp/s%,cpp/S%,$@) $@

cpp/Step1_read_print cpp/Step2_eval: $(STEP1_DEPS)
cpp/Step3_env: $(STEP3_DEPS)
cpp/Step4_if_fn_do cpp/Step5_tco cpp/Step6_file cpp/Step7_quote cpp/Step8_macros cpp/Step9_try cpp/StepA_mal: $(STEP4_DEPS)


# JavaScript target (js)

s%.js: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -js $@

JS_DEPS = node_readline.js node_modules
step0_repl.js: $(JS_DEPS)
step1_read_print.js step2_eval.js: $(STEP1_DEPS) $(JS_DEPS)
step3_env.js: $(STEP3_DEPS) $(JS_DEPS)
step4_if_fn_do.js step5_tco.js step6_file.js step7_quote.js step8_macros.js step9_try.js stepA_mal.js: $(STEP4_DEPS) $(JS_DEPS)

node_modules:
	npm install

###

clean:
	rm -r step*.py cpp/ step*.js step*.n
