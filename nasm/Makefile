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

step3_env: step3_env.asm $(COMPONENTS)
	nasm -felf64 step3_env.asm
	ld -o $@ step3_env.o

step4_if_fn_do: step4_if_fn_do.asm $(COMPONENTS)
	nasm -felf64 step4_if_fn_do.asm
	ld -o $@ step4_if_fn_do.o

step5_tco: step5_tco.asm $(COMPONENTS)
	nasm -felf64 step5_tco.asm
	ld -o $@ step5_tco.o

step6_file: step6_file.asm $(COMPONENTS)
	nasm -felf64 step6_file.asm
	ld -o $@ step6_file.o
