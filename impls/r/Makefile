SOURCES_BASE = readline.r types.r reader.r printer.r
SOURCES_LISP = env.r core.r stepA_mal.r
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

STEPS = step0_repl.r step1_read_print.r step2_eval.r step3_env.r \
	step4_if_fn_do.r step5_tco.r step6_file.r \
	step7_quote.r step8_macros.r step9_try.r stepA_mal.r

all: libs

dist: mal.r mal

mal.r: $(SOURCES)
	cat $+ | grep -v " source(" > $@

mal: mal.r
	echo "#!/usr/bin/env Rscript" > $@
	cat $< >> $@
	chmod +x $@

$(STEPS): libs

.PHONY:
libs: lib/rdyncall

lib/rdyncall:
	curl -O http://cran.r-project.org/src/contrib/Archive/rdyncall/rdyncall_0.7.5.tar.gz
	mkdir -p lib
	R CMD INSTALL rdyncall_0.7.5.tar.gz -l lib/
	rm rdyncall_0.7.5.tar.gz

clean:
	rm -f mal.r mal


