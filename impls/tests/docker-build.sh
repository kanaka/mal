#!/bin/bash

IMAGE_NAME=${IMAGE_NAME:-mal-test-ubuntu-utopic}
GIT_TOP=$(git rev-parse --show-toplevel)

docker build -t "${IMAGE_NAME}" "${GIT_TOP}/tests/docker"
