#!/bin/bash

set -ex

ACTION=${1}
IMPL=${2}

BUILD_IMPL=${BUILD_IMPL:-${IMPL}}

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
echo "BUILD_IMPL: ${BUILD_IMPL}"
echo "MAL_IMPL: ${MAL_IMPL}"

if [ "${MAL_IMPL}" ]; then
    if [ "${NO_SELF_HOST}" ]; then
        echo "Skipping ${ACTION} of ${MAL_IMPL} self-host"
        exit 0
    fi
    if [ "${ACTION}" = "perf" -a "${NO_SELF_HOST_PERF}" ]; then
        echo "Skipping only perf test for ${MAL_IMPL} self-host"
        exit 0
    fi
fi

MAKE="make ${mode_val:+${mode_var}=${mode_val}}"

# If NO_DOCKER is blank then launch use a docker image, otherwise use
# the Travis image/tools directly.
if [ -z "${NO_DOCKER}" ]; then
    img_impl=$(echo "${MAL_IMPL:-${IMPL}}" | tr '[:upper:]' '[:lower:]')
    # We could just use make DOCKERIZE=1 instead but that does add
    # non-trivial startup overhead for each step.
    MAKE="docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl} ${MAKE}"
fi

case "${ACTION}" in
build)
    # rpython often fails on step9 in compute_vars_longevity
    # so build step9, then continue wit the full build
    if [ "${BUILD_IMPL}" = "rpython" ]; then
        ${MAKE} -C "${BUILD_IMPL}" step9_try || true
    fi
    ${MAKE} -C ${BUILD_IMPL}
    ;;
test|perf)
    if ! ${MAKE} TEST_OPTS="--debug-file ../${ACTION}.err" \
            ${MAL_IMPL:+MAL_IMPL=${MAL_IMPL}} \
            ${ACTION}^${IMPL}; then
        # print debug-file on error
        cat ${ACTION}.err
        false
    fi
    ;;
esac


