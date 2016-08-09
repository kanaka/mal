SOURCES_BASE = readline.ck reader.ck printer.ck \
               types/MalObject.ck types/MalSubr.ck \
               types/**/*.ck util/*.ck
SOURCES_LISP = env.ck core.ck stepA_mal.ck
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

clean:

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all clean stats stats-lisp
