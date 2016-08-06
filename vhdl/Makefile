TESTS =

SOURCES_BASE = pkg_readline.vhdl types.vhdl printer.vhdl reader.vhdl
SOURCES_LISP = env.vhdl core.vhdl stepA_mal.vhdl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.vhdl step1_read_print.vhdl step2_eval.vhdl step3_env.vhdl \
       step4_if_fn_do.vhdl step5_tco.vhdl step6_file.vhdl step7_quote.vhdl \
       step8_macros.vhdl step9_try.vhdl stepA_mal.vhdl
OBJS = $(SRCS:%.vhdl=%.o)
BINS = $(OBJS:%.o=%)
OTHER_SRCS = pkg_readline.vhdl types.vhdl printer.vhdl reader.vhdl env.vhdl core.vhdl
OTHER_OBJS = $(OTHER_SRCS:%.vhdl=%.o)

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

work-obj93.cf: $(OTHER_SRCS)
	rm -f work-obj93.cf
	ghdl -i $+

$(OTHER_OBJS):  %.o: %.vhdl work-obj93.cf
	ghdl -a -g $(@:%.o=%.vhdl)

$(OBJS): %.o: %.vhdl $(OTHER_OBJS)
	ghdl -a -g $(@:%.o=%.vhdl)

$(patsubst %.o,%,$(filter step%,$(OBJS))): $(OTHER_OBJS)
$(BINS): %: %.o
	ghdl -e -g $@

# The ghdl linker creates a lowercase executable file; rename it to stepA_mal
stepA_mal: stepA_mal.o
	ghdl -e -g $@
	mv stepa_mal stepA_mal

clean:
	rm -f $(OBJS) $(BINS) $(OTHER_OBJS) work-obj93.cf mal

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
