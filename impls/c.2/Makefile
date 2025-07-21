CC = gcc

# Optimization is not required but enables some more warnings.
CFLAGS = -std=c99 -g -Wall -Wextra -O1

# The code defines new format specifiers.
CPPFLAGS = -Wno-format

S0 = step0_repl
S1 = step1_read_print
S2 = step2_eval
S3 = step3_env
S4 = step4_if_fn_do
S5 = step5_tco
S6 = step6_file
S7 = step7_quote
S8 = step8_macros
S9 = step9_try
SA = stepA_mal

S4+ := $(S4) $(S5) $(S6) $(S7) $(S8) $(S9) $(SA)
S3+ := $(S3) $(S4+)
S1+ := $(S1) $(S2) $(S3+)
S0+ := $(S0) $(S1+)

VPATH = libs/hashmap libs/linked_list

all: $(S0+)

# GCC could create temporary objects files, but separate recipes for
# .o objects give faster build cycles when debugging.

$(S0+): LDLIBS += -ledit

$(S1+): hashmap.o linked_list.o printer.o reader.o types.o
$(S1+): LDLIBS += -lgc

$(S3+): env.o

# ffi is only used by stepA, but we want the same core.o for all steps.
# Anyway, the --as-needed linker option is active by default.
$(S4+): core.o
$(S4+): LDLIBS += -ldl -lffi
core.o: CPPFLAGS += -DWITH_FFI

include deps
deps:
	gcc -MM -MF- *.c > $@

clean:
	rm -f $(S0+) *.o deps

.PHONY: all clean
