#!/bin/bash

set -ex

case ${TRAVIS_OS_NAME} in
linux)
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')

    docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${impl} make TEST_OPTS="--soft --log-file ../test.out" test^${IMPL}
    #docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${IMPL,,} make perf^${IMPL}
    ;;
osx)
    make TEST_OPTS="--soft --log-file ../test.out" test^${IMPL}
    #make TEST_OPTS="--soft --log-file ../perf" perf^${IMPL}
    ;;
esac
