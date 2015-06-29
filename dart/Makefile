
TESTS =

SOURCES_BASE =
SOURCES_LISP =
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: dart_packages

dart_packages:
	pub install

clean:
	@echo "Running (clean)..."

mal.dart:
	@echo "Running (mal.dart)..."

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@echo "Running (stats)..."

stats-lisp: $(SOURCES_LISP)
	@echo "Running (stats-lisp)..."

tests: $(TESTS)

$(TESTS):
	@echo "Running (TESTS)..."