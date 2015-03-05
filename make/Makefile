
TESTS = tests/types.mk tests/reader.mk tests/stepA_mal.mk

SOURCES_BASE = util.mk numbers.mk readline.mk gmsl.mk types.mk \
               reader.mk printer.mk
SOURCES_LISP = env.mk core.mk stepA_mal.mk
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

mal.mk: $(SOURCES)
	echo "#!/usr/bin/make -f" > $@
	cat $+ | grep -v "^include " >> $@
	chmod +x $@

clean:
	rm -f mal.mk

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	make -f $@ || exit 1; \
