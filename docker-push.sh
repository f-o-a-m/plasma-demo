#!/bin/bash
set -e
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
SHORTREF=`git rev-parse --short HEAD`
docker push foamspace/plasma-demo:latest
docker tag foamspace/plasma-demo:latest foamspace/plasma-demo:$SHORTREF
docker push foamspace/plasma-demo:$SHORTREF
