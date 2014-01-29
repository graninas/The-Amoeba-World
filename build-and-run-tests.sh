#!/bin/bash

set -e

./build-tests.sh "$@"
./run-tests.sh "$@"

set +e