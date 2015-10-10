#!/bin/bash

set -ex

# If NO_DOCKER is blank then launch use a docker image, otherwise
# use the Travis image/tools directly.
if [ -z "${NO_DOCKER}" ]; then
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')

    docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${impl} make TEST_OPTS="--soft --log-file ../test.out" test^${IMPL}
    #docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${IMPL,,} make perf^${IMPL}
else
    make TEST_OPTS="--soft --log-file ../test.out" test^${IMPL}
    #make TEST_OPTS="--soft --log-file ../perf" perf^${IMPL}
fi
