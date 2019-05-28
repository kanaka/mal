#!/bin/bash

set -ex

ACTION=${1}
IMPL=${2}

# Special tags/branches
case "${TRAVIS_BRANCH}" in
self-host-test)
    MAL_IMPL=${IMPL}
    IMPL=mal
    ;;
esac

mode_var=${MAL_IMPL:-${IMPL}}_MODE
mode_val=${!mode_var}

echo "ACTION: ${ACTION}"
echo "IMPL: ${IMPL}"
echo "MAL_IMPL: ${MAL_IMPL}"

if [ "${MAL_IMPL}" ]; then
    if [ "${NO_SELF_HOST}" ]; then
        echo "Skipping ${MAL_IMPL} self-host"
        exit 0
    fi
    if [ "${ACTION}" = "perf" -a "${NO_SELF_HOST_PERF}" ]; then
        echo "Skipping perf test for ${MAL_IMPL} self-host"
        exit 0
    fi
fi

# If NO_DOCKER is blank then launch use a docker image, otherwise use
# the Travis image/tools directly.
if [ "${NO_DOCKER}" ]; then
    MAKE="make"
else
    img_impl=$(echo "${MAL_IMPL:-${IMPL}}" | tr '[:upper:]' '[:lower:]')
    # We could just use make DOCKERIZE=1 instead but that does add
    # non-trivial startup overhead for each step.
    MAKE="docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl} make"
fi

${MAKE} TEST_OPTS="--debug-file ../${ACTION}.err" \
    ${MAL_IMPL:+MAL_IMPL=${MAL_IMPL}} \
     ${mode_val:+${mode_var}=${mode_val}} \
    ${ACTION}^${IMPL}

# no failure so remove error log
rm -f ${ACTION}.err || true
