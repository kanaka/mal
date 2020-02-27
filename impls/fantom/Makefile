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
