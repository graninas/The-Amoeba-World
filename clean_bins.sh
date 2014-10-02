#!/bin/bash

echo 'Clear once...'

PACKAGES=`ghc-pkg list | grep "Amoeba"` # | grep -v 'Logic' | grep -v 'Middleware'

for f in $PACKAGES
do
    PKG=`echo $f | sed 's/[{}]//g'`
    ghc-pkg unregister $PKG
done

# Sometimes, ghc-pkg can't remove some packages. Try again.

echo 'Clear again...'

PACKAGES=`ghc-pkg list | grep "Amoeba"` # | grep -v 'Logic' | grep -v 'Middleware'

for f in $PACKAGES
do
    PKG=`echo $f | sed 's/[{}]//g'`
    ghc-pkg unregister $PKG
done
