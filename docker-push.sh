#!/bin/bash
set -e
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
SHORTREF=`git rev-parse --short HEAD`
docker push foamspace/plasma-mvp-sidechain:latest
docker tag foamspace/plasma-mvp-sidechain:latest foamspace/plasma-mvp-sidechain:$SHORTREF
docker push foamspace/plasma-mvp-sidechain:$SHORTREF
