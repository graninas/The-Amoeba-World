#!/bin/bash

mkdir ./.bin
mkdir ./.bin/Amoeba
set -e

./scripts/build.sh
./scripts/copy.sh

set +e
