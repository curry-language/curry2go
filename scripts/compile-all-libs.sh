#!/bin/sh
# Generate intermediate files of all libraries until everything is compiled.
# This script must be executed in the `lib` directory!

VERSION=`jq -r '.version' ../package.json`
if [ -z "$VERSION" ] ; then
  echo "Variable VERSION undefined!"
  exit 1
fi
MAJORVERSION=`echo $VERSION | cut -d. -f1`
MINORVERSION=`echo $VERSION | cut -d. -f2`

FRONTEND=../bin/*-frontend
FRONTENDPARAMS="-o .curry/curry2go-$VERSION -D__CURRY2GO__=$MAJORVERSION$(printf "%02d" $MINORVERSION) -Ono-remove-unused-imports"

compile_all_targets() {
  "$FRONTEND" --flat                       $FRONTENDPARAMS $1
  "$FRONTEND" --type-annotated-flat --flat $FRONTENDPARAMS $1
  "$FRONTEND" --acy                        $FRONTENDPARAMS $1
  "$FRONTEND" --uacy                       $FRONTENDPARAMS $1
  "$FRONTEND" --comments                   $FRONTENDPARAMS $1
  "$FRONTEND" --ast                        $FRONTENDPARAMS $1
}

compile_module() {
  FILE=$1
  FILE=`expr $FILE : '\(.*\)\.curry' \| $FILE`
  MODNAME=`echo $FILE | tr '/' '.'`
  compile_all_targets $MODNAME
}

compile_all_modules() {
  for F in `find * -name "*.curry"` ; do
    compile_module $F
  done
}

compile_all_modules_to_Go() {
  for F in `find * -name "*.curry"` ; do
    F1=`expr $F : '\(.*\)\.curry' \| $F`
    MODNAME=`echo $F1 | tr '/' '.'`
    ../bin/curry2goc --compile $MODNAME
  done
}

# iterating the compilation of all libraries until no new compilations occur:
TMPOUT=TMPLIBOUT
CCODE=0
while [ $CCODE = 0 ] ; do
  echo "Compiling all modules..."
  compile_all_modules > $TMPOUT
  echo "NEW COMPILED LIBRARIES IN THIS ITERATION:"
  grep Compiling $TMPOUT
  CCODE=$?
done
/bin/rm -r $TMPOUT

# generate Go targets for all libraries:
compile_all_modules_to_Go

