#!/bin/bash

set -e

echo "Building..."
cd Amoeba
ghc -threaded --make -outputdir ../.bin -o ../.bin/Amoeba Amoeba.hs
cd ..

echo "Copying..."
rm ./Game/Amoeba
cp ./.bin/Amoeba ./Game/

set +e