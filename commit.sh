#!/bin/bash

# This is dangerous script, until I'll find a good solution.

git add -A
git commit -a -m "$1"
git push

cd ../The-Amoeba-World-AI/
git add -A
git commit -a -m "$1"
git push

cd ../The-Amoeba-World-GameLogic/
git add -A
git commit -a -m "$1"
git push

cd ../The-Amoeba-World-GameStorage/
git add -A
git commit -a -m "$1"
git push

cd ../The-Amoeba-World-Middleware/
git add -A
git commit -a -m "$1"
git push

cd ../The-Amoeba-World-View/
git add -A
git commit -a -m "$1"
git push

cd ../The-Amoeba-World/
