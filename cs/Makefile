#####################

DEBUG =

TESTS =

SOURCES_BASE = readline.cs types.cs reader.cs printer.cs
SOURCES_LISP = env.cs core.cs stepA_mal.cs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

OTHER_SOURCES = getline.cs

#####################

SRCS = step0_repl.cs step1_read_print.cs step2_eval.cs step3_env.cs \
       step4_if_fn_do.cs step5_tco.cs step6_file.cs step7_quote.cs \
       step8_macros.cs step9_try.cs stepA_mal.cs

LIB_SRCS = $(filter-out step%,$(OTHER_SOURCES) $(SOURCES))

FLAGS = $(if $(strip $(DEBUG)),-debug+,)

#####################

all: $(patsubst %.cs,%.exe,$(SRCS))

dist: mal.exe mal

mal.exe: $(patsubst %.cs,%.exe,$(word $(words $(SOURCES)),$(SOURCES)))
	cp $< $@

# NOTE/WARNING: static linking triggers mono libraries LGPL
# distribution requirements.
# http://www.mono-project.com/archived/guiderunning_mono_applications/
mal: $(patsubst %.cs,%.exe,$(word $(words $(SOURCES)),$(SOURCES))) mal.dll
	mkbundle --static -o $@ $+ --deps

mal.dll: $(LIB_SRCS)
	mcs $(FLAGS) -target:library $+ -out:$@

%.exe: %.cs mal.dll
	mcs $(FLAGS) -r:mal.dll $<

clean:
	rm -f mal *.dll *.exe *.mdb

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
