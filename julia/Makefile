
SOURCES_BASE = reader.jl printer.jl readline_mod.jl types.jl
SOURCES_LISP = env.jl core.jl stepA_mal.jl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
