all: step0_repl step1_read_print step2_eval

%: %.b malhdr.h
	cintsys -c "bcpl $< to $@"

.DELETE_ON_ERROR:

step0_repl: readline.b types.b
step1_read_print: printer.b reader.b readline.b types.b
step2_eval: printer.b reader.b readline.b types.b
step3_env: env.b printer.b reader.b readline.b types.b

clean:
	rm step0_repl step1_read_print
