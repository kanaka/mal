
TESTS = tests/types.mk tests/reader.mk tests/stepA_mal.mk

SOURCES_BASE = util.mk numbers.mk readline.mk gmsl.mk types.mk \
               reader.mk printer.mk
SOURCES_LISP = env.mk core.mk stepA_mal.mk
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.mk mal

mal.mk: $(SOURCES)
	cat $+ | grep -v "^include " > $@

mal: mal.mk
	echo "#!/usr/bin/make -f" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.mk mal

.PHONY: tests $(TESTS)

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	make -f $@ || exit 1; \
