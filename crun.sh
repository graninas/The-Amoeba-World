#!/bin/bash

cabal install ../The-Amoeba-World-Middleware
cabal install ../The-Amoeba-World-GameLogic
cabal install ../The-Amoeba-World-GameStorage
cabal install ../The-Amoeba-World-AI
cabal install ../The-Amoeba-World-View
cabal install ./

cabal run
