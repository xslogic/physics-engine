#!/bin/bash

rm -rf build
mkdir build
mkdir build/hidir
mkdir build/odir
mkdir build/bin

for x in `find src -type f -depth 1 | awk -F"/" '{print $2}'`
do   
	PROG=`basename $x | awk -F"." '{print $1}'`
	cd src ; ghc --make $x -hidir ../build/hidir -odir ../build/odir -o ../build/bin/$PROG; cd ..
done
