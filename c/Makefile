USE_READLINE ?=
USE_GC ?= 1
CFLAGS += -g -O2
LDFLAGS += -g

#####################

TESTS =

SOURCES_BASE = readline.h readline.c types.h types.c \
               reader.h reader.c printer.h printer.c \
               interop.h interop.c
SOURCES_LISP = env.c core.h core.c stepA_mal.c
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


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


ifeq ($(shell uname -s),Darwin)
CFLAGS +=-DOSX=1
endif

ifeq (,$(USE_READLINE))
RL_LIBRARY ?= edit
else
RL_LIBRARY ?= readline
CFLAGS += -DUSE_READLINE=1
endif

ifeq (,$(USE_GC))
else
CFLAGS += -DUSE_GC=1
LDFLAGS += -lgc
endif

CFLAGS += $(GLIB_CFLAGS)
LDFLAGS += -l$(RL_LIBRARY) $(GLIB_LDFLAGS) -ldl -lffi 

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(OBJS) $(OTHER_OBJS): %.o: %.c  $(OTHER_HDRS)
	gcc $(CFLAGS) -c $(@:%.o=%.c) -o $@

$(patsubst %.o,%,$(filter step%,$(OBJS))): $(OTHER_OBJS)
$(BINS): %: %.o
	gcc $+ -o $@ $(LDFLAGS)

clean:
	rm -f $(OBJS) $(BINS) $(OTHER_OBJS) mal

.PHONY: stats stats-lisp tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	./$@ || exit 1; \
