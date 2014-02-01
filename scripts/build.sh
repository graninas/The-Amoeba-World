#!/bin/bash

echo "Building..."
cd Amoeba
ghc -threaded --make -outputdir ../.bin -o ../.bin/Amoeba Main.hs
cd ..

