#####################

SOURCES_BASE = types.nim reader.nim printer.nim
#SOURCES_LISP = env.nim core.nim stepA_interop.nim
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.nim step1_read_print.nim
BINS = $(SRCS:%.nim=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	#nimble build
	cp $< $@

define dep_template
$(1): $(SOURCES_BASE) $(1).nim
	nim c $$@
endef
$(foreach b,$(BINS),$(eval $(call dep_template,$(b))))

clean:
	rm -rf nimcache/ $(BINS)
	rm -f mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
#stats-lisp: $(SOURCES_LISP)
#	@wc $^
