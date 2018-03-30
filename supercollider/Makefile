SOURCES_BASE = reader.sc printer.sc types.sc
SOURCES_LISP = env.sc core.sc stepA_mal.sc
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: sclang_conf.yaml

sclang_conf.yaml:
	@printf 'includePaths:\n  - $(CURDIR)/lib\n' > sclang_conf.yaml

clean:
	$(RM) sclang_conf.yaml

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all clean stats stats-lisp
