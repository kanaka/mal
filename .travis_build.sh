#!/bin/bash

set -ex

case ${TRAVIS_OS_NAME} in
linux)
    BUILD_IMPL=${BUILD_IMPL:-${IMPL}}
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')
    build_impl=$(echo "${BUILD_IMPL}" | tr '[:upper:]' '[:lower:]')

    docker pull kanaka/mal-test-${impl}
    if [ "${impl}" != "${build_impl}" ]; then
        docker pull kanaka/mal-test-${build_impl}
    fi
    docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${build_impl} make -C ${BUILD_IMPL}
    ;;
osx)
    make -C ${IMPL}
    ;;
esac
