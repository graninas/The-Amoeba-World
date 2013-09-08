#!/bin/bash

#echo Building tests...

#rm -rf "./.bin/TestBin/*"
#rm -rf "./.bin/Test/*"

cd ./Amoeba/Test/
FILES=`ls | grep Test`
cd ..

#for f in $FILES
#do
#    ghc -threaded -outputdir ../.bin/TestBin -o ../.bin/Test/${f%%.hs}.bin ./Test/$f
#done

#chmod u+x ../.bin/Test/*.bin
cd ..

echo Running tests...

cd ./.bin/Test/
FILES=`ls`

# TODO: print failed cases as well.
for f in $FILES
do
    echo $f
    ./$f | egrep -wi 'Failed|All tests passed.|==='
done

cd ../..