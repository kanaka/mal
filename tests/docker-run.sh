#!/bin/bash

IMAGE_NAME=${IMAGE_NAME:-mal-test-ubuntu-utopic}
GIT_TOP=$(git rev-parse --show-toplevel)
 
docker run -it --rm -u ${EUID} \
    --volume=${GIT_TOP}:/mal \
    ${IMAGE_NAME} \
    "${@}"
