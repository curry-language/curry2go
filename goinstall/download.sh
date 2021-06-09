#!/bin/sh -x
#
# This is the Curry2Go installation script.
#
# To install the Curry2Go system in directory /opt/Curry2Go, you must be
# able to run the `sudo` command to get admin rights. Then run:
#
#    > curl -sSL https://www.informatik.uni-kiel.de/~mh/curry2go/download.sh | sh
#
# or:
#
#    > wget -qO- https://www.informatik.uni-kiel.de/~mh/curry2go/download.sh | sh
#
# In order to install a local Curry2Go system in directory C2GDIR
# (which does not not `sudo` right but needs more time since the complete
# Curry2Go system will be compiled on the local machine), add an argument:
#
#    > curl ... | sh -s - -d C2GDIR
#
# If the additional argument `--nocypm` is provided,  the Curry Package Manager
# CPM is not installed with the local Curry2Go system (which is reasonable
# if you already have CPM installed).
#
##############################################################################

# Some URLs and locations:
OPTC2GTARURL=https://www.informatik.uni-kiel.de/~mh/curry2go/opt-curry2go.tgz
TMPC2GTARURL=https://www.informatik.uni-kiel.de/~mh/curry2go/tmp-curry2go.tgz
OPTC2GDIR=/opt/Curry2Go
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

# Download and install Curry2Go in /opt/Curry2Go (requires sudo):
installopt() {
  echo "sudo required to install into /opt/Curry2Go:"
  sudo mkdir -m 755 -p $OPTC2GDIR
  cd $OPTC2GDIR
  sudo /bin/rm -rf Curry2Go
  echo "Downloading and installing Curry2Go in directory '$OPTC2GDIR'..."
  curl -sSL $OPTC2GTARURL | sudo tar xz
  sudo chown -R root:root Curry2Go
  sudo chmod 755 Curry2Go
  cd Curry2Go
  sudo env "PATH=$PATH" "GOPATH=`pwd`/go" make GOCURRYWORKSPACE=`pwd`/go/src/gocurry installdist
  sudo chmod -R go+rX .
  cd ..
  sudo /bin/rm -f bin
  sudo ln -s Curry2Go/bin bin

  echo "Add '$OPTC2GDIR/bin' to your path to use the"
  echo "installed Curry2Go system, e.g., by"
  echo ""
  echo "    > export PATH=$OPTC2GDIR/bin:\$PATH"
  echo ""
}

# Download and install Curry2Go in /tmp/Curry2Go:
installtmp() {
  /bin/rm -rf $TMPC2GDIR
  echo "Downloading and installing Curry2Go in directory '$TMPC2GDIR'..."
  cd /tmp
  curl -sSL $TMPC2GTARURL | tar xz
  cd $TMPC2GDIR && make installdist
}

if [ -z "$INSTALLDIR" ] ; then
  installopt
else
  installtmp

  # include temporary Curry2Go in path:
  PATH=$TMPC2GDIR/bin:$PATH
  export PATH

  echo "Configuration of Curry2Go CPM:"
  cypm  -d CURRYBIN=$TMPC2GDIR/bin/curry2go config

  # Install a local Curry2Go system in directory INSTALLDIR:
  git clone $C2GURL $INSTALLDIR
  cd $INSTALLDIR
  cypm update # get newest packages
  make CURRYSYSTEM=$TMPC2GDIR/bin/curry2go bootstrap

  if [ $LOCALCPM = yes ] ; then
    # Install a local version of CPM:
    cypm -d CURRYBIN=$INSTALLDIR/bin/curry2go install cpm
  fi

  echo "Add '$INSTALLDIR/bin' to your path to use the"
  echo "installed Curry2Go system, e.g., by"
  echo ""
  echo "    > export PATH=$INSTALLDIR/bin:\$PATH"
  echo ""
fi
