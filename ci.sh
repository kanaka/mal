#!/usr/bin/env bash

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

log_prefix="${ACTION}${REGRESS:+-regress}-${IMPL}${mode_val:+-${mode_val}}${MAL_IMPL:+-${MAL_IMPL}}"
TEST_OPTS="${TEST_OPTS} --debug-file ../../${log_prefix}.debug"

step_summary() {
    echo "${*}"
    if [ "${GITHUB_STEP_SUMMARY}" ]; then
        echo "${*}" >> "${GITHUB_STEP_SUMMARY}"
    fi
}

img_base="${MAL_IMPL:-${IMPL}}"
img_impl="${img_base%%-mal}"
img_name="mal-test-$(echo "${img_impl}" | tr '[:upper:]' '[:lower:]')"
img_ver=$(./voom-like-version.sh impls/${img_impl}/Dockerfile)
IMAGE="ghcr.io/kanaka/${img_name}:${img_ver}"

# If NO_DOCKER is blank then run make in a docker image
MAKE="make ${mode_val:+${mode_var}=${mode_val}}"
if [ -z "${NO_DOCKER}" ]; then
    # We could just use make DOCKERIZE=1 instead but that does add
    # non-trivial startup overhead for each step.
    MAKE="docker run -i -u $(id -u) -v `pwd`:/mal ${IMAGE} ${MAKE}"
fi

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
echo "IMAGE: ${IMAGE}"
echo "MAKE: ${MAKE}"

case "${ACTION}" in
docker-build-push)
    if ! docker pull ${IMAGE}; then
        step_summary "${BUILD_IMPL} - building ${IMAGE}"
        make "docker-build^${BUILD_IMPL}"
        step_summary "${BUILD_IMPL} - built ${IMAGE}"
        if [ "${GITHUB_REPOSITORY}" = "kanaka/mal" ] && [ "${GITHUB_REF}" = "refs/heads/master" ]; then
            docker push ${IMAGE}
            step_summary "${BUILD_IMPL} - pushed ${IMAGE}"
        fi
    fi
    ;;
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
        # show debug-file path on error
        echo "Full debug log is at: ${log_prefix}.debug"
        false
    fi
    ;;
esac
