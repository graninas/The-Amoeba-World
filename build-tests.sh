#!/bin/bash

rm -rf "./.bin/TestBin/"*
rm -rf "./.bin/Test/"*

./copy-test-data.sh

cd ./Amoeba/Test/

if [ "$1" != "" ]; then
TESTS=`echo "$@" | tr " " "|"`
echo Tests to build: "$@"
FILES=`ls | egrep -i "$TESTS" | grep Test`
else
echo Building all tests...
FILES=`ls | grep Test`
fi

cd ..

for f in $FILES
do
    ghc -threaded -outputdir ../.bin/TestBin -o ../.bin/Test/${f%%.hs}.bin ./Test/$f | grep -v Loading
done

chmod u+x ../.bin/Test/*.bin
cd ..