TESTS =

SOURCES_BASE = readline.rexx types.rexx reader.rexx printer.rexx
SOURCES_LISP = env.rexx core.rexx stepA_mal.rexx
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

SRCS = step0_repl.rexx step1_read_print.rexx step2_eval.rexx step3_env.rexx \
       step4_if_fn_do.rexx step5_tco.rexx step6_file.rexx step7_quote.rexx \
       step8_macros.rexx step9_try.rexx stepA_mal.rexx
PREPROCESSED = $(SRCS:%.rexx=%.rexxpp)

all: $(PREPROCESSED) dist

dist: mal

mal: mal.rexxpp
	echo "#!/usr/bin/rexx -a" > $@
	cat $< >> $@
	chmod +x $@

mal.rexxpp: stepA_mal.rexxpp
	cp -a $+ $@

$(PREPROCESSED): %.rexxpp: %.rexx readline.rexx types.rexx reader.rexx printer.rexx env.rexx core.rexx
	cpp -CC -P -nostdinc $< > $@

clean:
	rm -f mal.rexx mal *.rexxpp

.PHONY: all dist clean stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	rexx $@ || exit 1; \
