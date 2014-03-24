
TESTS = tests/types.mk tests/reader.mk tests/step9_interop.mk

SOURCES = util.mk readline.mk gmsl.mk types.mk reader.mk stepA_more.mk

mal.mk: $(SOURCES)
	echo "#!/usr/bin/make -f" > $@
	cat $+ | grep -v "^include " >> $@
	chmod +x $@

clean:
	rm -f mal.mk

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	make -f $@ || exit 1; \
