all: step0_repl

%: %.b malhdr.h
	cintsys -c "bcpl $< to $@"

step0_repl: readline.b

clean:
	rm step0_repl
