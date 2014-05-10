#!/bin/bash

echo "Cleaning..."

rm -fr "./.bin/*"

mkdir "./.bin/Test"
mkdir "./.bin/TestBin"

./build.sh
