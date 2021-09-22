#!/bin/sh
#
# This is the Curry2Go installation script.
#
# To install the Curry2Go system in the local directory `Curry2Go`, run:
#
#    > curl -sSL https://www-ps.informatik.uni-kiel.de/curry2go/download.sh | sh
#
# or:
#
#    > wget -qO- https://www-ps.informatik.uni-kiel.de/curry2go/download.sh | sh
#
# In order to install into a (non-existing!) directory C2GDIR, add an argument:
#
#    > curl ... | sh -s - -d C2GDIR
#
##############################################################################

# URL of the distribution:
DOWNLOADURL=https://www-ps.informatik.uni-kiel.de/curry2go/download

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
INSTALLDIR=Curry2Go # directory to install local Curry2Go system
BUILDFRONTEND=no    # compile and install front end from the repository?

# check arguments for appropriate settings:
while [ $# -gt 0 -a -z "$ERROR" ]; do
  case $1 in
    --help   | -h | -\? ) HELP=yes ;;
    --dir    | -d       ) shift; INSTALLDIR=$1 ;;
    --frontend          ) BUILDFRONTEND=yes ;;
    --version           ) shift; VERSION=$1 ;;
    -*                  ) ERROR="Unknown option: $1" ;;
  esac
  shift
done

if [ -n "$ERROR" ] ; then
  echo "ERROR: $ERROR"
  exit 1
fi

if [ $BUILDFRONTEND = yes ] ; then
  STACK=`which stack`
  if [ ! -x "$STACK" ] ; then
    echo "Executable 'stack' (required for front end install) not found!"
    echo "Please install 'stack' and try again!"
    exit 1
  fi
fi

if [ -e "$INSTALLDIR" ] ; then
  echo "Cannot install into directory '$INSTALLDIR' since it already exists!"
  exit 1
fi
mkdir -p "$INSTALLDIR"
INSTALLDIR=`cd $INSTALLDIR && pwd`

if [ $HELP = yes ] ; then
  echo "Usage: $0 [options]"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|-?|--help   : show this message and quit"
  echo "-d|--dir <DIR> : install into directory <DIR>"
  echo "--frontend     : compile front end from the sources"
  echo "--version <V>  : download version <V> instead of current version"
  echo "                 (e.g., '2021-07-03-1.0.0')"
  exit
fi

##############################################################################

installed_message() {
  INSTDIR=$1
  echo "--------------------------------------------------------------"
  echo "Curry2Go system is installed in '$INSTDIR'."
  echo "Add '$INSTDIR/bin' to your path for ease of use, e.g., by"
  echo ""
  echo "    > export PATH=$INSTDIR/bin:\$PATH"
  echo ""
  echo "Then start the Curry2Go REPL by the command"
  echo ""
  echo "    > curry2go [-n|--nocypm]"
  echo ""
  echo "(if you do not use packages, use '-n' or '--nocypm' for faster startup)"
}

# Download and install Curry2Go in $INSTALLDIR:
install_from_tar() {
  echo "Downloading and installing Curry2Go into '$INSTALLDIR'..."
  cd $INSTALLDIR && curl -sSL $DOWNLOADURL/curry2go.tgz | tar xz
  cd $INSTALLDIR && make BUILDFRONTEND=$BUILDFRONTEND installdist
}

install_from_tar
installed_message `cd $INSTALLDIR && pwd`
