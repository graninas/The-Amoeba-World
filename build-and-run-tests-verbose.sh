#!/bin/bash

set -e

./build-tests.sh "$@"
./run-tests-verbose.sh "$@"

set +e