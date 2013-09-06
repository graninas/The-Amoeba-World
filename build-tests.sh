#!/bin/bash

echo Building tests...

rm -rf "./bin/TestBin/*"
rm -rf "./bin/Test/*"

cd ./Amoeba/Test/
FILES=`ls`
cd ..

for f in $FILES
do
    ghc -threaded -outputdir ../bin/TestBin -o ../bin/Test/${f%%.hs}.bin ./Test/$f
done

chmod u+x ../bin/Test/*.bin
cd ..