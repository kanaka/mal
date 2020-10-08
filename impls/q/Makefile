.DEFAULT:
	echo

OBJECTS := step0_repl_mixed.q \
	step1_read_print_mixed.q \
	step2_eval_mixed.q \
	step3_env_mixed.q

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

clean:
	rm $(OBJECTS)

FORCE:
