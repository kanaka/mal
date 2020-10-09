.DEFAULT:
	echo

OBJECTS := step0_repl_mixed.q \
	step1_read_print_mixed.q \
	step2_eval_mixed.q \
	step3_env_mixed.q \
	step4_if_fn_do_mixed.q \
	step5_tco_mixed.q \
	step6_file_mixed.q \
	step7_quote_mixed.q \
	step8_macros_mixed.q \
	step9_try_mixed.q \
	stepA_mal_mixed.q

all: $(OBJECTS)
	echo "hello there general kenobi"

step0_repl_mixed.q: FORCE
	./mix.sh step0_repl utils

step1_read_print_mixed.q: FORCE
	./mix.sh step1_read_print utils reader printer

step2_eval_mixed.q: FORCE
	./mix.sh step2_eval utils reader printer

step3_env_mixed.q: FORCE
	./mix.sh step3_env utils reader printer env

step4_if_fn_do_mixed.q: FORCE
	./mix.sh step4_if_fn_do utils reader printer env core

step5_tco_mixed.q: FORCE
	./mix.sh step5_tco utils reader printer env core

step6_file_mixed.q: FORCE
	./mix.sh step6_file utils reader printer env core atom

step7_quote_mixed.q: FORCE
	./mix.sh step7_quote utils reader printer env core atom

step8_macros_mixed.q: FORCE
	./mix.sh step8_macros utils reader printer env core atom

step9_try_mixed.q: FORCE
	./mix.sh step9_try utils reader printer env core atom

stepA_mal_mixed.q: FORCE
	./mix.sh stepA_mal utils reader printer env core atom

clean:
	rm $(OBJECTS)

FORCE:
