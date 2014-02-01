#!/bin/bash

cd ./.bin/Test/

if [ "$1" != "" ]; then
TESTS=`echo "$@" | tr " " "|"`
echo Tests to run \(verbose\): "$@"
FILES=`ls | egrep -i "$TESTS" | grep Test`
else
echo Running all tests \(verbose\)...
FILES=`ls`
fi

for f in $FILES
do
    echo $f
    ./$f
done

cd ../..