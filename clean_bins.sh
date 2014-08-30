#!/bin/bash

echo 'Clear once...'

PACKAGES=`ghc-pkg list | grep "Amoeba"`

for f in $PACKAGES
do
    PKG=`echo $f | sed 's/[{}]//g'`
    ghc-pkg unregister $PKG
done

# Sometimes, ghc-pkg can't remove some packages. Try again.

echo 'Clear again...'

PACKAGES=`ghc-pkg list | grep "Amoeba"`

for f in $PACKAGES
do
    PKG=`echo $f | sed 's/[{}]//g'`
    ghc-pkg unregister $PKG
done
