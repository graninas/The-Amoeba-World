#!/bin/bash

set -e

./scripts/build.sh
./scripts/copy.sh

set +e