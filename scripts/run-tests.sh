#!/bin/bash

cd ./.bin/Test/

if [ "$1" != "" ]; then
TESTS=`echo "$@" | tr " " "|"`
echo Tests to run: "$@"
FILES=`ls | egrep -i "$TESTS" | grep Test`
else
echo Running all tests...
FILES=`ls`
fi

for f in $FILES
do
    echo $f
    ./$f | egrep -wi 'Failed|All tests passed.|==='
done

cd ../..