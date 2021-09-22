#!/bin/sh
# adapt in a given file the location of root of the Curry2Go system:

ROOT=$(dirname $(dirname $(realpath $0)))
C2GDISTDIR=$1
FILE=$2

cat $FILE | sed "s|$C2GDISTDIR|$ROOT|g"  > $FILE.tmp
mv $FILE.tmp $FILE
