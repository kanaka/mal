SOURCES = types.wren env.wren printer.wren reader.wren readline.wren interop.wren core.wren stepA_mal.wren

all:
	true

dist: mal

mal.wren: $(SOURCES)
	cat $+ | grep -v '^import "./' > $@

mal: mal.wren
	echo "#!/usr/bin/env wren" > $@
	cat $< >> $@
	chmod +x $@

.PHONY: clean

clean:
	rm -f mal.wren mal
