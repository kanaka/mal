USE_READLINE ?=
USE_GC ?= 1
CFLAGS ?= -g -O2
LDFLAGS ?= -g

#####################

SRCS = step0_repl.c step1_read_print.c step2_eval.c step3_env.c \
       step4_if_fn_do.c step5_tco.c step6_file.c step7_quote.c \
       step8_macros.c step9_try.c stepA_mal.c
OBJS = $(SRCS:%.c=%.o)
BINS = $(OBJS:%.o=%)
OTHER_OBJS = types.o readline.o reader.o printer.o env.o core.o interop.o
OTHER_HDRS = types.h readline.h reader.h printer.h       core.h interop.h

GLIB_CFLAGS ?= $(shell pkg-config --cflags glib-2.0)
GLIB_LDFLAGS ?= $(shell pkg-config --libs glib-2.0)

FFI_CFLAGS ?= $(shell pkg-config libffi --cflags)
FFI_LDFLAGS ?= $(shell pkg-config libffi --libs)

ifeq ($(shell uname -s),Darwin)
  darwin_CPPFLAGS ?= -DOSX=1
endif

ifeq (,$(USE_READLINE))
RL_LIBRARY ?= edit
else
RL_LIBRARY ?= readline
  rl_CFLAGS ?= -DUSE_READLINE=1
endif

ifneq (,$(USE_GC))
  gc_CFLAGS ?= -DUSE_GC=1
  gc_LIBS ?= -lgc
endif

# Rewrite CPPFLAGS for the Make recipes, but let existing user options
# take precedence.
override CPPFLAGS := \
  ${darwin_CPPFLAGS} ${rl_CFLAGS} ${gc_CFLAGS} ${GLIB_CFLAGS} ${FFI_CFLAGS} \
  ${CPPFLAGS}
override LDLIBS += \
  ${gc_LIBS} -l${RL_LIBRARY} ${GLIB_LDFLAGS} ${FFI_LDFLAGS} -ldl

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(OBJS) $(OTHER_OBJS): %.o: %.c  $(OTHER_HDRS)

$(patsubst %.o,%,$(filter step%,$(OBJS))): $(OTHER_OBJS)
$(BINS): %: %.o

clean:
	rm -f $(OBJS) $(BINS) $(OTHER_OBJS) mal
