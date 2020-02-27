SOURCES_BASE = readline.php types.php reader.php printer.php interop.php
SOURCES_LISP = env.php core.php stepA_mal.php
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

dist: mal.php mal

mal.php: $(SOURCES)
	cat $+ | grep -v "^require_once" > $@

mal: mal.php
	echo "#!/usr/bin/env php" > $@
	cat $< >> $@
	chmod +x $@

mal-web.php: mal.php
	cat $< | ( IFS="NON-MATCHING-IFS"; while read -r line; do if [ "$$line" = "// run mal file" ]; then echo "?>"; cat webrunner.php; echo "<?php"; fi; printf '%s\n' "$$line"; done < "$<" ) > $@

clean:
	rm -f mal.php mal mal-web.php
