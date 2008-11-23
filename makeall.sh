#!/bin/bash


for x in `find src -type f -depth 1 | awk -F"/" '{print $2}'`
do   
	PROG=`basename $x | awk -F"." '{print $1}'`
	PDIR=$PROG
	rm -rf $PDIR
	mkdir $PDIR
	mkdir $PDIR/hidir
	mkdir $PDIR/odir
	mkdir $PDIR/bin
	echo "cd src ; ghc --make $x -hidir ../$PDIR/hidir -odir ../$PDIR/odir -o ../$PDIR/bin/runme; cd .."
	cd src ; ghc --make $x -hidir ../$PDIR/hidir -odir ../$PDIR/odir -o ../$PDIR/bin/runme; cd ..
done
