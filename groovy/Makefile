CLASSES = types.class reader.class printer.class env.class core.class

SOURCES_BASE = types.groovy reader.groovy printer.groovy
SOURCES_LISP = env.groovy core.groovy stepA_mal.groovy
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: ${CLASSES}

step1_read_print.groovy: types.class reader.class printer.class
step2_eval.groovy: types.class reader.class printer.class
step3_env.groovy: types.class reader.class printer.class env.class
step4_if_fn_do.groovy step6_file.groovy step7_quote.groovy step8_macros.groovy step9_try.groovy stepA_mal.groovy: ${CLASSES}

types.class: types.groovy
	groovyc $<

env.class: env.groovy
	groovyc $<

reader.class: reader.groovy
	groovyc $<

printer.class: printer.groovy
	groovyc $<

core.class: core.groovy types.class reader.class printer.class
	groovyc $<

clean:
	rm -f *.class

.PHONY: stats tests

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
