#!/bin/bash

set -ex

ACTION=${1}
IMPL=${2}
MAL_IMPL=${3:-js}

echo "ACTION: ${ACTION}"
echo "IMPL: ${IMPL}"
echo "MAL_IMPL: ${MAL_IMPL}"

# If NO_DOCKER is blank then launch use a docker image, otherwise
# use the Travis image/tools directly.
if [ -z "${NO_DOCKER}" ]; then
    impl=$(echo "${IMPL}" | tr '[:upper:]' '[:lower:]')
    img_impl=$(echo "${3:-${IMPL}}" | tr '[:upper:]' '[:lower:]')

    docker run -it -u $(id -u) -v `pwd`:/mal kanaka/mal-test-${img_impl} \
        make TEST_OPTS="--soft --log-file ../${ACTION}.err" \
            MAL_IMPL=${MAL_IMPL} ${ACTION}^${IMPL}
else
    make TEST_OPTS="--soft --log-file ../${ACTION}.err" \
        MAL_IMPL=${MAL_IMPL} ${ACTION}^${IMPL}
fi
# no failure so remove error log
rm ${ACTION}.err
