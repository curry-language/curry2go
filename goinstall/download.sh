#!/bin/sh
#
# This is the Curry2Go installation script.
#
# To install the Curry2Go system in directory /tmp/Curry2Go, run:
#
#    > curl -sSL https://www.informatik.uni-kiel.de/~mh/curry2go/download.sh | sh
#
# or:
#
#    > wget -qO- https://www.informatik.uni-kiel.de/~mh/curry2go/download.sh | sh
#
# In order to install a local Curry2Go system in directory C2GDIR
# (which needs more time since the complete Curry2Go system will be
# compiled on the local machine), add an argument:
#
#    > curl ... | sh -s - -d C2GDIR
#
# The additional argument `--nocypm` does not install the Curry Package Manager
# CPM with the local Curry2Go system (which is reasonable if you already
# have CPM installed).
#
##############################################################################

# Some URLs and locations:
C2GTARURL=https://www.informatik.uni-kiel.de/~mh/curry2go/tmpcurry2go.tgz
C2GTARFILE=/tmp/curry2go.tgz
TMPC2GDIR=/tmp/Curry2Go
C2GURL=https://git.ps.informatik.uni-kiel.de/curry/curry2go.git

##############################################################################

# check required tools
CURL=`which curl`
if [ ! -x "$CURL" ] ; then
  echo "Required executable 'curl' not found!"
  echo "Please install 'curl' and try again!"
  exit 1
fi
GO=`which go`
if [ ! -x "$GO" ] ; then
  echo "Required executable 'go' not found!"
  echo "Please install Go compiler and try again!"
  exit 1
fi

ERROR=
HELP=no
INSTALLDIR=   # directory to install local Curry2Go system
LOCALCPM=yes  # install local version of CPM with local Curry2Go systen?

# check arguments for appropriate settings:
while [ $# -gt 0 -a -z "$ERROR" ]; do
  case $1 in
    --help  | -h | -\? ) HELP=yes ;;
    --dir   | -d       ) shift; INSTALLDIR=$1 ;;
    --nocpm | -n       ) LOCALCPM=no ;;
    -*                 ) ERROR="Unknown option: $1" ;;
  esac
  shift
done

if [ -n "$INSTALLDIR" ] ; then
  if [ -d "$INSTALLDIR" ] ; then
    echo "ERROR: installation directory '$INSTALLDIR' already exists!"
    exit 1
  fi
  mkdir -p "$INSTALLDIR"
  INSTALLDIR=`cd $INSTALLDIR && pwd`
  rmdir "$INSTALLDIR"
fi

if [ $HELP = yes ] ; then
  echo "Usage: $0 [options]"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|-?|--help   : show this message and quit"
  echo "-d|--dir <DIR> : install a local Curry2Go system in directory <DIR>"
  echo "-n|--nocpm     : do not install CPM with local Curry2Go system"
  exit
fi
#echo INSTALLDIR=$INSTALLDIR
#echo LOCALCPM=$LOCALCPM

##############################################################################

# Download and install Curry2Go in /tmp:
/bin/rm -rf $TMPC2GDIR $C2GTARFILE
echo "Downloading and installing Curry2Go in directory '$TMPC2GDIR'..."
curl -o $C2GTARFILE $C2GTARURL
cd /tmp && tar xzf curry2go.tgz
/bin/rm $C2GTARFILE
cd $TMPC2GDIR && make installdist

# include temporary Curry2Go in path:
PATH=$TMPC2GDIR/bin:$PATH
export PATH

echo "Configuration of Curry2Go CPM:"
cypm  -d CURRYBIN=$TMPC2GDIR/bin/curry2go config

if [ -n "$INSTALLDIR" ] ; then
  # Install a local Curry2Go system in directory INSTALLDIR:
  git clone $C2GURL $INSTALLDIR
  cd $INSTALLDIR
  cypm update # get newest packages
  make CURRYSYSTEM=$TMPC2GDIR/bin/curry2go # create initial compiler
  make bootstrap # to base Curry2Go on local libs and tools (front end)

  if [ $LOCALCPM = yes ] ; then
    # Install a local version of CPM:
    cypm -d CURRYBIN=$INSTALLDIR/bin/curry2go install cpm
  fi

  echo "Add '$INSTALLDIR/bin' to your path to use the local Curry2Go system!"
fi
