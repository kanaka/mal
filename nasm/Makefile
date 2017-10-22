step0_repl: step0_repl.asm
	nasm -felf64 step0_repl.asm
	ld -o $@ step0_repl.o

COMPONENTS=core.asm reader.asm printer.asm types.asm system.asm

step1_read_print: step1_read_print.asm $(COMPONENTS)
	nasm -felf64 step1_read_print.asm
	ld -o $@ step1_read_print.o

step2_eval: step2_eval.asm $(COMPONENTS)
	nasm -felf64 step2_eval.asm
	ld -o $@ step2_eval.o
