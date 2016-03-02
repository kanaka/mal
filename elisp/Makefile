SOURCES_BASE = reader.el printer.el types.el
SOURCES_LISP = env.el func.el core.el stepA_mal.el
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

clean:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

