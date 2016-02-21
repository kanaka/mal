#####################

DEBUG = 

TESTS =

SOURCES_BASE = types.fs error.fs node.fs printer.fs tokenizer.fs reader.fs \
	       readline.fs
SOURCES_LISP = core.fs env.fs stepA_mal.fs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

TERMINAL_SOURCES = terminal.cs

#####################

SRCS = step0_repl.fs step1_read_print.fs step2_eval.fs step3_env.fs \
       step4_if_fn_do.fs step5_tco.fs step6_file.fs step7_quote.fs \
       step8_macros.fs step9_try.fs stepA_mal.fs
DLL_SOURCES = $(filter-out stepA_mal.fs,$(SOURCES))

FSFLAGS = $(if $(strip $(DEBUG)),--debug+,--debug- --optimize+ --tailcalls+)
CSFLAGS = $(if $(strip $(DEBUG)),-debug+,)
#####################

all: $(patsubst %.fs,%.exe,$(SRCS))

Mono.Terminal.dll: $(TERMINAL_SOURCES)
	mcs $(CSFLAGS) -target:library $+ -out:$@

mal.dll: $(DLL_SOURCES) Mono.Terminal.dll
	fsharpc $(FSFLAGS) -o $@ -r Mono.Terminal.dll -a $(DLL_SOURCES)

%.exe: %.fs mal.dll
	fsharpc $(FSFLAGS) -o $@ -r mal.dll $<

clean:
	rm -f *.dll *.exe *.mdb

.PHONY: stats tests $(TESTS)

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
