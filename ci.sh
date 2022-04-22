#!/bin/bash

set -ex

ACTION=${1}
IMPL=${2}

die() { local ret=$1; shift; echo >&2 "${*}"; exit $ret; }

# Environment variable configuration
BUILD_IMPL=${BUILD_IMPL:-${IMPL}}

if [ "${DO_SELF_HOST}" ]; then
    MAL_IMPL=${IMPL}
    IMPL=mal
fi

if [ "${DO_HARD}" ]; then
    TEST_OPTS="${TEST_OPTS} --hard"
fi

raw_mode_var=${MAL_IMPL:-${IMPL}}_MODE
mode_var=${raw_mode_var/-/__}
mode_var=${mode_var/./__}
mode_val=${!mode_var}

MAKE="make ${mode_val:+${mode_var}=${mode_val}}"

log_prefix="${ACTION}${REGRESS:+-regress}-${IMPL}${mode_val:+-${mode_val}}${MAL_IMPL:+-${MAL_IMPL}}"
TEST_OPTS="${TEST_OPTS} --debug-file ../../${log_prefix}.debug"

# Log everything below this point:
exec &> >(tee ./${log_prefix}.log)

if [ "${NO_PERF}" -a "${ACTION}" = "perf" ]; then
    die 0 "Skipping perf test"
fi
if [ "${NO_SELF_HOST}" -a "${DO_SELF_HOST}" ]; then
    die 0 "Skipping ${ACTION} of ${MAL_IMPL} self-host"
fi
if [ "${NO_SELF_HOST_PERF}" -a "${DO_SELF_HOST}" -a "${ACTION}" = "perf" ]; then
    die 0 "Skipping only perf test for ${MAL_IMPL} self-host"
fi

echo "ACTION: ${ACTION}"
echo "IMPL: ${IMPL}"
echo "BUILD_IMPL: ${BUILD_IMPL}"
echo "MAL_IMPL: ${MAL_IMPL}"
echo "TEST_OPTS: ${TEST_OPTS}"

# If NO_DOCKER is blank then launch use a docker image, otherwise use
# the Travis/Github Actions image/tools directly.
if [ -z "${NO_DOCKER}" ]; then
    img_impl=$(echo "${MAL_IMPL:-${IMPL}}" | tr '[:upper:]' '[:lower:]')
    # We could just use make DOCKERIZE=1 instead but that does add
    # non-trivial startup overhead for each step.
    MAKE="docker run -i -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl%%-mal} ${MAKE}"
fi

case "${ACTION}" in
build)
    # rpython often fails on step9 in compute_vars_longevity
    # so build step9, then continue with the full build
    if [ "${BUILD_IMPL}" = "rpython" ]; then
        ${MAKE} -C "impls/${BUILD_IMPL}" step9_try || true
    fi
    ${MAKE} -C "impls/${BUILD_IMPL}"
    ;;
test|perf)
    [ "${ACTION}" = "perf" ] && STEP=
    if ! ${MAKE} TEST_OPTS="${TEST_OPTS}" \
            ${MAL_IMPL:+MAL_IMPL=${MAL_IMPL}} \
            ${REGRESS:+REGRESS=${REGRESS}} \
            ${HARD:+HARD=${HARD}} \
            ${DEFERRABLE:+DEFERRABLE=${DEFERRABLE}} \
            ${OPTIONAL:+OPTIONAL=${OPTIONAL}} \
            ${ACTION}^${IMPL}${STEP:+^${STEP}}; then
        # print debug-file on error
        cat ${log_prefix}.debug
        false
    fi
    ;;
esac
