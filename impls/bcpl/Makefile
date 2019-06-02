all: step0_repl step1_read_print

%: %.b malhdr.h
	cintsys -c "bcpl $< to $@"

.DELETE_ON_ERROR:

step0_repl: readline.b types.b
step1_read_print: reader.b readline.b types.b

clean:
	rm step0_repl step1_read_print
