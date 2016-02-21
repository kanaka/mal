#!/bin/bash

set -ex

ACTION=${1}
IMPL=${2}
MAL_IMPL=${3:-js}

echo "ACTION: ${ACTION}"
echo "IMPL: ${IMPL}"
echo "MAL_IMPL: ${MAL_IMPL}"

# If NO_DOCKER is blank then launch use a docker image, otherwise use
# the Travis image/tools directly.
if [ "${NO_DOCKER}" ]; then
    MAKE="make"
else
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')
    img_impl=$(echo "${3:-${IMPL}}" | tr '[:upper:]' '[:lower:]')

    MAKE="docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl} make"
fi

${MAKE} TEST_OPTS="--debug-file ../${ACTION}.err" \
    MAL_IMPL=${MAL_IMPL} ${ACTION}^${IMPL}

# no failure so remove error log
rm -f ${ACTION}.err || true
