#!/usr/bin/env bash

STEP=${1:-stepE_macros}

CP=$(gradle -q --console plain printClasspath)
native-image --macro:truffle --no-fallback --initialize-at-build-time \
	-H:+TruffleCheckBlackListedMethods \
	-cp "$CP" truffle.mal.$STEP build/$STEP
