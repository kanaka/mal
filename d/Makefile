CFLAGS += -g -O2 -Wall
LDFLAGS += -lreadline

#####################

TESTS =

SOURCES_BASE = readline.d types.d reader.d printer.d
SOURCES_LISP = env.d mal_core.d stepA_mal.d
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

EARLY_SRCS = step0_repl.d step1_read_print.d step2_eval.d
LATE_SRCS = step3_env.d step4_if_fn_do.d step5_tco.d step6_file.d \
	    step7_quote.d step8_macros.d step9_try.d stepA_mal.d
SRCS = $(EARLY_SRCS) $(LATE_SRCS)
OBJS = $(SRCS:%.d=%.o)
BINS = $(OBJS:%.o=%)
EARLY_OBJS = types.o readline.o reader.o printer.o env.o
OTHER_OBJS = $(EARLY_OBJS) mal_core.o
EARLY_STEPS_BINS = $(EARLY_SRCS:%.d=%)
LATE_STEPS_BINS = $(LATE_SRCS:%.d=%)

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(OBJS) $(OTHER_OBJS): %.o: %.d
	gdc $(CFLAGS) -c $(@:%.o=%.d) -o $@

$(EARLY_STEPS_BINS): $(EARLY_OBJS)
$(LATE_STEPS_BINS): $(OTHER_OBJS)

$(BINS): %: %.o
	gdc $+ -o $@ $(LDFLAGS)

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
