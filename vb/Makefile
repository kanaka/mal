#####################

DEBUG =

TESTS =

SOURCES_BASE = readline.vb types.vb reader.vb printer.vb
SOURCES_LISP = env.vb core.vb stepA_mal.vb
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.vb step1_read_print.vb step2_eval.vb \
       step3_env.vb step4_if_fn_do.vb step5_tco.vb step6_file.vb \
       step7_quote.vb step8_macros.vb step9_try.vb stepA_mal.vb

LIB_CS_SRCS = getline.cs
LIB_VB_SRCS = $(filter-out step%,$(filter %.vb,$(SOURCES)))

FLAGS = $(if $(strip $(DEBUG)),-debug:full,)

#####################

all: mal.exe $(patsubst %.vb,%.exe,$(SRCS))

mal.exe: $(patsubst %.vb,%.exe,$(word $(words $(SOURCES)),$(SOURCES)))
	cp $< $@

mal_cs.dll: $(LIB_CS_SRCS)
	mcs $(FLAGS) -target:library $+ -out:$@

mal_vb.dll: mal_cs.dll $(LIB_VB_SRCS)
	vbnc $(FLAGS) -target:library -r:mal_cs.dll $(LIB_VB_SRCS) -out:$@

%.exe: %.vb mal_vb.dll
	vbnc $(FLAGS) -r:mal_vb.dll -r:mal_cs.dll $<

clean:
	rm -f *.dll *.exe *.mdb

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*'|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*'|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	./$@ || exit 1; \
