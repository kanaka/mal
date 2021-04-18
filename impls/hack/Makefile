STEPS = step0_repl.hack step1_read_print.hack step2_eval.hack step3_env.hack \
	step4_if_fn_do.hack step5_tco.hack step6_file.hack \
	step7_quote.hack step8_macros.hack step9_try.hack stepA_mal.hack

bin/$(STEPS): install_vendor

all: install_vendor

install_vendor:
	# https://getcomposer.org/
	composer install

clean:
	rm -rf vendor


