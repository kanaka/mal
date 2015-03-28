#!/bin/bash

IMAGE_NAME=mal-cpp
CONTAINER_NAME=mal-cpp-running

run() {
    docker rm -f $CONTAINER_NAME > /dev/null 2>/dev/null
    docker run -v $PWD:/mal -ti --name $CONTAINER_NAME $IMAGE_NAME "$@"
}

case $1 in

    build)
        docker build -t $IMAGE_NAME .
        ;;

    run)
        shift
        run "$@"
        ;;

    make)
        shift
        run make "$@"
        ;;

    *)
        echo "usage: $0 [build|run|make]"
        exit 1

        ;;

esac

