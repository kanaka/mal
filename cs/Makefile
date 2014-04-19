#####################

DEBUG =

TESTS =

SOURCES_BASE = readline.cs types.cs reader.cs printer.cs
SOURCES_LISP = env.cs core.cs stepA_more.cs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

OTHER_SOURCES = getline.cs

#####################

SRCS = step0_repl.cs step1_read_print.cs step2_eval.cs step3_env.cs \
       step4_if_fn_do.cs step5_tco.cs step6_file.cs step7_quote.cs \
       step8_macros.cs stepA_more.cs

LIB_SRCS = $(filter-out step%,$(OTHER_SOURCES) $(SOURCES))

FLAGS = $(if $(strip $(DEBUG)),-debug+,)

#####################

all: mal.exe $(patsubst %.cs,%.exe,$(SRCS))

mal.exe: $(patsubst %.cs,%.exe,$(word $(words $(SOURCES)),$(SOURCES)))
	cp $< $@

mal.dll: $(LIB_SRCS)
	mcs $(FLAGS) -target:library $+ -out:$@

%.exe: %.cs mal.dll
	mcs $(FLAGS) -r:mal.dll $<

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
