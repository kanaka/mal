SOURCES_BASE = src/mallib/fan/interop.fan src/mallib/fan/reader.fan src/mallib/fan/types.fan
SOURCES_LISP = src/mallib/fan/env.fan src/mallib/fan/core.fan src/stepA_mal/fan/main.fan
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: dist

dist: lib/fan/mal.pod

lib/fan:
	mkdir -p $@

lib/fan/mal.pod: lib/fan/stepA_mal.pod
	cp -a $< $@

lib/fan/step%.pod: src/step%/build.fan src/step%/fan/*.fan lib/fan/mallib.pod
	FAN_ENV=util::PathEnv FAN_ENV_PATH=. fan $<

lib/fan/mallib.pod: src/mallib/build.fan src/mallib/fan/*.fan lib/fan
	FAN_ENV=util::PathEnv FAN_ENV_PATH=. fan $<

clean:
	rm -rf lib

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
