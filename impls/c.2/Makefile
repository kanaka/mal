CC = gcc

CFLAGS = -std=c99 -g -Wall -Wextra -fanalyzer

# The code defines new format specifiers.
CPPFLAGS = -Wno-format

ifdef debug_reader
  CPPFLAGS += -DDEBUG_READER
endif
ifdef debug_hash
  CPPFLAGS += -DDEBUG_HASH
endif
ifdef debug_hashmap
  CPPFLAGS += -DDEBUG_HASHMAP
endif
ifdef debug_hash_collisions
  CPPFLAGS += -DDEBUG_HASH_COLLISIONS
endif
ifndef no_fast
  CFLAGS  += -flto -O3 -DNDEBUG
  LDFLAGS += -flto
endif
ifdef profilie
  CFLAGS  += -pg
  LDFLAGS += -pg
endif
ifdef readline
  pkgconfig_modules += readline
  CFLAGS += -DUSE_READLINE
else
  pkgconfig_modules += libedit
endif
ifndef no_ffi
  pkgconfig_modules += libffi
  CFLAGS += -DWITH_FFI
endif

pkgconfig_modules += bdw-gc
CFLAGS += $(shell pkg-config --cflags $(pkgconfig_modules))
LDLIBS += $(shell pkg-config --libs   $(pkgconfig_modules))

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

all: $(S0+)

# GCC could create temporary objects files, but separate recipes for
# .o objects give faster build cycles when debugging.
$(S0+): readline.o
$(S1+): error.o hashmap.o linked_list.o printer.o reader.o types.o vector.o
$(S3+): env.o
$(S4+): core.o

include deps
deps:
	$(CC) -MM -MF- *.c > $@

clean:
	rm -f $(S0+) *.o deps

.PHONY: all clean
