
TESTS =


SOURCES = src/main/java/mal/readline.java src/main/java/mal/types.java \
          src/main/java/mal/reader.java src/main/java/mal/printer.java \
          src/main/java/mal/env.java src/main/java/mal/core.java \
          src/main/java/mal/stepA_more.java

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^

#tests: $(TESTS)
#
#$(TESTS):
#	@echo "Running $@"; \
#	python $@ || exit 1; \
