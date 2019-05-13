PROGRAMS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
           step5_tco step6_file step7_quote step8_macros step9_try stepA_mal
AUX1 = gc.vala types.vala reader.vala printer.vala
AUX3 = $(AUX1) env.vala
AUX4 = $(AUX3) core.vala

# Inhibit default make rules, in case they try to build from leftover .c files
.SUFFIXES:

all: $(PROGRAMS)

# You can define VFLAGS on the command line to add flags to the vala compiler.
# Some useful ones:
#
#  -g        annotate the output C with #line directives so that backtraces
#            from gdb, sanitisers and valgrind will list Vala source locations
#
#  -X -g -X -O0    compile the output C for sensible debugging
#
#  -X -fsanitize=address    link the output program against Address Sanitizer
#
#  --save-temps    don't automatically delete the C files after compiling
#
#  -D GC_STATS     print statistics every time the garbage collector runs
#
#  -D GC_DEBUG     print full diagnostics from the garbage collector
#
#  -D GC_ALWAYS    make the garbage collector run at every opportunity
#                  (good for making occasional GC errors show up sooner)

$(PROGRAMS): %: %.vala
	valac $(VFLAGS) -o $@ $^ $(DEFINES) --pkg readline -X -lreadline

step1_read_print step2_eval: override DEFINES += -D NO_ENV

step0_repl:
step1_read_print: $(AUX1)
step2_eval: $(AUX1)
step3_env: $(AUX3)
step4_if_fn_do: $(AUX4)
step5_tco: $(AUX4)
step6_file: $(AUX4)
step7_quote: $(AUX4)
step8_macros: $(AUX4)
step9_try: $(AUX4)
stepA_mal: $(AUX4)

clean: clean-c
	rm -f $(PROGRAMS)

clean-c:
	rm -f *.c *.h
