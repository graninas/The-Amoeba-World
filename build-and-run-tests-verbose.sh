#!/bin/bash

rm -rf "./.bin/TestBin/"*
rm -rf "./.bin/Test/"*

cd ./Amoeba/Test/

if [ "$1" != "" ]; then
TESTS=`echo "$@" | tr " " "|"`
echo Tests to build and run \(verbose\): "$@"
FILES=`ls | egrep -i "$TESTS" | grep Test`
else
echo Building and running all tests \(verbose\)...
FILES=`ls | grep Test`
fi

cd ..

for f in $FILES
do
    ghc -threaded -outputdir ../.bin/TestBin -o ../.bin/Test/${f%%.hs}.bin ./Test/$f | grep -v Loading
done

chmod u+x ../.bin/Test/*.bin

cd ../.bin/Test/

if [ "$1" != "" ]; then
TESTS=`echo "$@" | tr " " "|"`
FILES=`ls | egrep -i "$TESTS" | grep Test`
else
FILES=`ls`
fi

for f in $FILES
do
    echo $f
    ./$f
done

cd ../..