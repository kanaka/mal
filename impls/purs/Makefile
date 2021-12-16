BINS = step0_repl.js step1_read_print.js step2_eval.js step3_env.js \
       step4_if_fn_do.js step5_tco.js step6_file.js step7_quote.js \
       step8_macros.js step9_try.js stepA_mal.js

OTHER_SRCS = src/Readline.js src/Readline.purs src/Types.purs src/Reader.purs \
             src/Printer.purs src/Env.purs src/Core.purs


all: $(BINS)

$(BINS): %.js: src/%.purs $(OTHER_SRCS) node_modules/readline-sync
	spago bundle-app --main $($(<:src/%=%)) --to $@


node_modules/readline-sync:
	npm install


#####################

step0_repl.purs = Mal.Step0
step1_read_print.purs = Mal.Step1
step2_eval.purs = Mal.Step2
step3_env.purs = Mal.Step3
step4_if_fn_do.purs = Mal.Step4
step5_tco.purs = Mal.Step5
step6_file.purs = Mal.Step6
step7_quote.purs = Mal.Step7
step8_macros.purs = Mal.Step8
step9_try.purs = Mal.Step9
stepA_mal.purs = Mal.StepA


clean:
	rm -rf step*.js output/*