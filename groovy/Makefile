CLASSES = types.class reader.class printer.class env.class core.class

SOURCES_BASE = types.groovy reader.groovy printer.groovy
SOURCES_LISP = env.groovy core.groovy stepA_mal.groovy
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: ${CLASSES}

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
