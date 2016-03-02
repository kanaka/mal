TESTS =

SOURCES_BASE = lib/types/types.factor lib/reader/reader.factor lib/printer/printer.factor
SOURCES_LISP = lib/env/env.factor lib/core/core.factor stepA_mal/stepA_mal.factor
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.factor mal

# dependency order (env must come before types)
ORDERED_SOURCES = $(filter %env.factor,$(SOURCES)) $(filter-out %env.factor,$(SOURCES))
mal.factor: $(ORDERED_SOURCES)
	cat $+ |  sed '/^USING:/,/;/ s/ *lib.[a-z]*//g' > $@

mal: mal.factor
	echo '#!/usr/bin/env factor' > $@
	cat $< >> $@
	chmod +x $@

# TODO: standalone compiled app
#mal.factor: $(SOURCES)
#	mkdir -p dist_tmp; \
#	FDIR=$$(dirname $$(readlink -f $$(which factor))); \
#	for f in $${FDIR}/*; do ln -sf $$f dist_tmp/; done; \
#	rm dist_tmp/factor; \
#	cp $${FDIR}/factor dist_tmp/factor; \
#	HOME=/mal FACTOR_ROOTS=. dist_tmp/factor dist.factor
#	#cat $+ | sed 's///' >> $@

clean:
	rm -f mal.factor

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\!|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\!|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

