
STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

COMPONENTS = env.asm core.asm reader.asm printer.asm types.asm system.asm exceptions.asm


all: $(STEPS)

%.o: %.asm $(COMPONENTS)
	nasm -felf64 $<

%: %.o
	ld -o $@ $<

.PHONY: clean
clean:
	rm -f $(STEPS) $(STEPS:%=%.o)
