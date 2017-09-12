#!/bin/bash

set -ex

BUILD_IMPL=${BUILD_IMPL:-${IMPL}}

mode_var=${IMPL}_MODE
mode_val=${!mode_var}

# If NO_DOCKER is blank then launch use a docker image, otherwise
# use the Travis image/tools directly.
if [ -z "${NO_DOCKER}" ]; then
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')
    img_impl=$(echo "${BUILD_IMPL}" | tr '[:upper:]' '[:lower:]')

    docker pull kanaka/mal-test-${impl}
    if [ "${impl}" != "${img_impl}" ]; then
        docker pull kanaka/mal-test-${img_impl}
    fi
    if [ "${BUILD_IMPL}" = "rpython" ]; then
        # rpython often fails on step9 in compute_vars_longevity
        # so build step9, then continue wit the full build
        docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl} \
            make -C ${BUILD_IMPL} step9_try || true
    fi
    docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl} \
        make ${mode_val:+${mode_var}=${mode_val}} \
            -C ${BUILD_IMPL}
else
    make ${mode_val:+${mode_var}=${mode_val}} \
        -C ${BUILD_IMPL}
fi
