#!/bin/sh
# shell script to run the Docker image caups/curry2go
# with appropriate options in order to use Curry2Go
# with local files and invoke tools contained in the image

# set docker options:
# run interactive, remove container after execution
DOCKEROPTS="-it --rm"
# mount current working directory and user's home directory
DOCKEROPTS="$DOCKEROPTS -v `pwd`:`pwd` -w `pwd` -v $HOME:$HOME -e HOME=$HOME"
# set docker user to host user
DOCKEROPTS="$DOCKEROPTS -u $(id -u):$(id -g)"

DOCKERTAG="caups/curry2go"
ENTRYPOINT=""
HELP=no

case $1 in
  --help | -h | -\? ) HELP=yes ;;
esac

if [ $HELP = yes ] ; then
  echo "Usage: curry2go-docker.sh [-h|-?|--help] [-t TAG] [options]"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|-?|--help       : show this message and quit"
  echo "-t TAG             : use docker image with tag TAG (default: caups/curry2go)"
  echo "cypm <opts>        : invoke Curry Package Manager with <opts>"
  echo "curry-check <opts> : invoke CurryCheck with <opts>"
  echo "curry-doc   <opts> : invoke CurryDoc with <opts>"
  echo "curry2go <opts>    : invoke Curry2Go with <opts>"
  echo "<opts>             : invoke Curry2Go with <opts>"
  exit
fi

# check docker image tag:
if [ $# -gt 1 -a "$1" = "-t" ] ; then
  shift ; DOCKERTAG=$1 ; shift
fi

# Options when the REPL is invoked:
REPLOPTS="--bincypm"

# check whether an installed tool should be invoked:
case $1 in
  curry2go          ) shift ;;
  cypm              ) shift ; ENTRYPOINT="/curry2go/Curry2Go/bin/cypm" ;;
  curry-check       ) shift ; ENTRYPOINT="/curry2go/cpm/bin/curry-check" ;;
  curry-doc         ) shift ; ENTRYPOINT="/curry2go/cpm/bin/curry-doc" ;;
esac

if [ -n "$ENTRYPOINT" ] ; then
  REPLOPTS=
  DOCKEROPTS="$DOCKEROPTS --entrypoint=$ENTRYPOINT"
fi

docker run $DOCKEROPTS $DOCKERTAG $REPLOPTS ${1+"$@"}
