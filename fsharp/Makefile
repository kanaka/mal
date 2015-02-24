#####################

DEBUG =

TESTS =

SOURCES_BASE = types.fs reader.fs printer.fs readline.fs
SOURCES_LISP =
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

TERMINAL_SOURCES = terminal.cs

#####################

SRCS = step0_repl.fs step1_read_print.fs

FLAGS = $(if $(strip $(DEBUG)),-debug+,)

#####################

all: $(patsubst %.fs,%.exe,$(SRCS))

Mono.Terminal.dll: $(TERMINAL_SOURCES)
	mcs $(FLAGS) -target:library $+ -out:$@

mal.dll: $(SOURCES) Mono.Terminal.dll
	fsharpc $(FLAGS) -o $@ -r Mono.Terminal.dll -a $(SOURCES)

%.exe: %.fs mal.dll
	fsharpc $(FLAGS) -o $@ -r mal.dll $<

clean:
	rm -f *.dll *.exe *.mdb

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	./$@ || exit 1; \
