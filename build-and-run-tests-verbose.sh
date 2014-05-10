#!/bin/bash

set -e

./scripts/clean-test-data.sh "$@"
./scripts/copy-test-data.sh
./scripts/build-tests.sh "$@"
./scripts/run-tests-verbose.sh "$@"

set +e