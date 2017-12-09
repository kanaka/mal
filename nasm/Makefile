
COMPONENTS=core.asm reader.asm printer.asm types.asm system.asm

%.o: %.asm $(COMPONENTS)
	nasm -felf64 $<

%: %.o
	ld -o $@ $<

