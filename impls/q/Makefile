.DEFAULT:
	echo

OBJECTS := step0_repl_mixed.q \
	step1_read_print_mixed.q

all: 
	echo "hello there general kenobi"

step0_repl_mixed.q: FORCE
	./mix.sh step0_repl utils

step1_read_print_mixed.q: FORCE
	./mix.sh step1_read_print utils reader printer

clean:
	rm $(OBJECTS)

FORCE:
