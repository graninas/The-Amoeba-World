#!/bin/bash

set -e

echo "Cleaning..."

rm -fr "./.bin/*"

./build.sh
