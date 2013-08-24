#!/bin/bash

cd Amoeba
ghc -threaded --make -outputdir ../bin -o ../Temp/Amoeba Amoeba.hs
cd ..

