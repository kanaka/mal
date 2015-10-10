#!/bin/bash

set -ex

BUILD_IMPL=${BUILD_IMPL:-${IMPL}}

# If NO_DOCKER is blank then launch use a docker image, otherwise
# use the Travis image/tools directly.
if [ -z "${NO_DOCKER}" ]; then
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')
    build_impl=$(echo "${BUILD_IMPL}" | tr '[:upper:]' '[:lower:]')

    docker pull kanaka/mal-test-${impl}
    if [ "${impl}" != "${build_impl}" ]; then
        docker pull kanaka/mal-test-${build_impl}
    fi
    if [ "${BUILD_IMPL}" = "rpython" ]; then
        # rpython often fails once per build in compute_vars_longevity
        # so build it twice and ignore failures the first time
        docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${build_impl} make -C ${BUILD_IMPL} || true
    fi
    docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${build_impl} make -C ${BUILD_IMPL}
else
    make -C ${BUILD_IMPL}
fi
