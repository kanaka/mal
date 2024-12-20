SOURCES_BASE = \
  Readline.pm \
  Types.pm \
  Reader.pm \
  Printer.pm \
  Interop.pm
SOURCES_LISP = \
  Env.pm \
  Core.pm \
  stepA_mal.pl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

dist: mal.pl mal

mal.pl: $(SOURCES)
	#fatpack pack ./stepA_mal.pl > $@
	fatpack trace ./stepA_mal.pl
	fatpack packlists-for `cat fatpacker.trace` > packlists
	fatpack tree `cat packlists`
	cp $+ fatlib/
	(fatpack file; cat ./stepA_mal.pl) > mal.pl

mal: mal.pl
	echo "#!/usr/bin/env perl" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.pl mal fatpacker.trace packlists fatlib/* *-lint
	[ -d fatlib ] && rmdir fatlib || true

no_critic := \
  ErrorHandling::RequireCarping \
  RequireVersionVar \
  # EOL

lint-all: $(addsuffix -lint,$(wildcard *.pl *.pm))
lint: $(SOURCES:%=%-lint)
%-lint: % Makefile
	perl -c -I. $*
	perltidy -st $* | diff -u $* -
	perlcritic -1 --verbose 11 $(no_critic:%=--exclude=%) $*
	touch $@
