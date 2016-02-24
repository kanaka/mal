SOURCES_BASE = reader.kt printer.kt types.kt env.kt core.kt readline.kt
SOURCES_LISP = step0_repl.kt step1_read_print.kt step2_eval.kt step3_env.kt step4_if_fn_do.kt \
	step5_tco.kt step6_file.kt step7_quote.kt step8_macros.kt step9_try.kt stepA_mal.kt

SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)
JARS = $(SOURCES_LISP:%.kt=%.jar)

all: $(JARS)

dist: mal.jar mal

mal.jar: stepA_mal.jar
	cp $< $@

SHELL := bash
mal: mal.jar
	cat <(echo -e '#!/bin/sh\nexec java -jar "$$0" "$$@"') mal.jar > $@
	chmod +x mal

clean:
	rm -vf $(JARS) mal.jar mal

$(JARS): %.jar: src/mal/%.kt $(SOURCES_BASE:%.kt=src/mal/%.kt)
	kotlinc src/mal/$(@:%.jar=%.kt) $(SOURCES_BASE:%.kt=src/mal/%.kt) -include-runtime -d $@

.PHONY: stats

stats: $(SOURCES:%.kt=src/mal/%.kt)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

stats-lisp: $(SOURCES_LISP:%.kt=src/mal/%.kt)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
