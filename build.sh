#!/bin/bash

cd Amoeba
ghc -threaded --make -outputdir ../bin -o ../bin/Amoeba Amoeba.hs
cd ..

