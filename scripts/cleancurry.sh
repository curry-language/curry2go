#!/bin/sh
# delete auxiliary files of Curry programs in a directory

RM=/bin/rm

HELP=no
case $1 in
  --help   | -h | -\? ) HELP=yes ;;
  -*                  ) echo "Unknown option: $1" ; HELP=yes ;;
esac

if [ $HELP = yes ] ; then
  echo "Usage: $0 [<prog> <prog> ...]" >&2
  echo "<prog>: remove only auxiliary Curry files for program <prog>" >&2
  exit 1
fi

# remove a given directory if it is empty:
remove_empty_dir() {
  DIR=$1
  if [ -d $DIR ] ; then
    DIRFILES=`ls -A $DIR`
    if [ -z "$DIRFILES" ] ; then
      rmdir $DIR
    fi
  fi
}

if [ $# = 0 ] ; then
  $RM -rf .curry/curry2go-*
  remove_empty_dir .curry
  exit
fi
  
for F in $* ; do
  if [ "$F" != "*.curry" -a "$F" != "*.lcurry" ] ; then
    F=`expr $F : '\(.*\)\.lcurry' \| $F`
    F=`expr $F : '\(.*\)\.curry' \| $F`
    FDIR=`dirname $F`
    FBASE=`basename $F | tr '.' '/'`
    CURRYDIR=$FDIR/.curry
    COMPILERDIRS=$CURRYDIR/curry2go-*
    for COMPILERDIR in $COMPILERDIRS ; do
      if [ -d $COMPILERDIR ] ; then
        CURRYF=$COMPILERDIR/$FBASE
        $RM -rf $CURRYF $CURRYF.*
        remove_empty_dir $COMPILERDIR
      fi
    done
    remove_empty_dir $CURRYDIR
  fi
done
