TESTS =

SOURCES_BASE = readline.r types.r reader.r printer.r
SOURCES_LISP = env.r core.r stepA_mal.r
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: libs

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


.PHONY:
libs: lib/rdyncall

lib/rdyncall:
	curl -O http://cran.r-project.org/src/contrib/Archive/rdyncall/rdyncall_0.7.5.tar.gz
	mkdir -p lib
	R CMD INSTALL rdyncall_0.7.5.tar.gz -l lib/
	rm rdyncall_0.7.5.tar.gz
