
SOURCES_BASE = reader.jl printer.jl readline_mod.jl types.jl
SOURCES_LISP = env.jl core.jl stepA_mal.jl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^
