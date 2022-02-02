#!/bin/sh
#
# Start interactive read-eval-print loop of Curry2Go

# Unless the first argument is '--trace', stack traces are not shown
# in case of errors:
if [ $# != 0 -a "$1" = "--trace" ] ; then
  shift
else
  GOTRACEBACK=none
  export GOTRACEBACK
fi

CURRY2GOHOME=$(dirname $(dirname $(realpath $0)))
export CURRY2GOHOME

# The bin directory of CURRY2GO:
CURRY2GOBIN=$CURRY2GOHOME/bin

# The directory where CPM installs the binaries:
CPMBIN="$HOME/.cpm/bin"

# check whether a requested tool is installed.
# If yes, execute it, otherwise exit with error.
check_and_exec_tool() {
  TOOLNAME=$1
  TOOLBIN="$CURRY2GOBIN/curry2go-$TOOLNAME"
  if [ -x "$TOOLBIN" ] ; then
    shift
    if [ "$TOOLNAME" = cypm ] ; then
      TOOLOPTS="-d curry_bin=$CURRY2GOBIN/curry2go"
    else
      TOOLOPTS=
    fi
    exec "$TOOLBIN" $TOOLOPTS ${1+"$@"}
  else
    echo "Incomplete installation: '$TOOLBIN' not installed!"
    exit 1
  fi
}

# check whether a tool of the distribution should be executed
case $1 in
  cypm | frontend ) check_and_exec_tool ${1+"$@"} ;;
esac

# Use readline wrapper rlwrap for REPL if rlwrap exists,
# we have tty as stdin, and we have a home dir to store rlwrap's history:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
fi

QUIET=no
USECPM=yes # should we call CPM to compute the correct load path?
BINCYPM=no

# check and remove arguments that should not be passed to the REPL:
for arg do
  shift
  case $arg in
    --bincypm     ) BINCYPM=yes  ;;
    --anycypm     ) BINCYPM=no   ;;
    --nocypm | -n ) USECPM=no    ;;
    --noreadline  ) USERLWRAP=no ;;
    *             ) set -- "$@" "$arg" ;;
  esac
done
#echo "ARGUMENTS PASSED TO REPL:"
#printf '%s\n' "$@"

# check REPL arguments that are relevant for this shell script:
for i in $* ; do
  case $i in
    --help | -h | -\? ) USECPM=no ;;
    --version | -V    ) USECPM=no ;;
    --numeric-version | --compiler-name | --base-version ) USECPM=no ;;
    --quiet  | -q     ) QUIET=yes ;;
  esac
done

CYPMBIN=
# if USECPM=yes, set variable CYPMBIN to the binary of CPM
if [ $USECPM = yes ] ; then
  if [ ! -d "$HOME" ] ; then      # do not use CPM without a home directory
    CYPMBIN=
  elif [ $BINCYPM = yes ] ; then  # use local binary of CPM
    CYPMBIN=$CURRY2GOBIN/cypm
  elif [ -x $CPMBIN/cypm ] ; then # use ~/.cpm/bin/cypm
    CYPMBIN=$CPMBIN/cypm
  else                            # use another binary of CPM
    WHICHCPM=`which cypm`
    if [ -x "$WHICHCPM" ] ; then
      CYPMBIN=$WHICHCPM
    fi
  fi
fi

REPL="$CURRY2GOHOME/bin/curry2goi"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $CURRY2GOHOME && make" >&2
  exit 1
fi

# Title/version of CPM passed to Curry2Go:
CPMVERSION=

if [ -n "$CYPMBIN" ] ; then
  # set CURRYPATH with 'deps' command of CPM
  if [ $QUIET = no ] ; then
    echo "Compute CURRYPATH with '$CYPMBIN'..."
  fi
  CPMPATH=`"$CYPMBIN" -v quiet -d CURRYBIN="$REPL" deps -p`
  if [ $? -gt 0 ] ; then
    echo $CPMPATH
    exit 1
  fi
  if [ -n "$CURRYPATH" ] ; then
    CURRYPATH=$CURRYPATH:$CPMPATH # keep existing CURRYPATH setting
  else
    CURRYPATH=$CPMPATH
  fi
  export CURRYPATH
  # set version string of CPM
  CPMVERSION=`"$CYPMBIN" -V`
  if [ $? -gt 0 ] ; then
    CPMVERSION=
  fi
fi

# delete possible remaining Maincurry2go files if their processes do not exist:
clean_maincurry() {
  ECODE=$?
  for i in Maincurry2go* ; do
    MPID=`expr $i : 'Maincurry2go\([0-9]*\).*'`
    if [ -n "$MPID" ] ; then
      if [ -z "`ps -p $MPID | fgrep $MPID`" ] ; then
        /bin/rm -f $i
      fi
    fi
  done
  exit $ECODE
}

# clean remaining Maincurry2go files in case of signals
trap 'clean_maincurry' 1 2 3 6

# do not use rlwrap inside Emacs:
if [ "$TERM" = dumb ] ; then
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  sleep 0.01 # to avoid aynchronous terminal setting problems with docker
  rlwrap -c -f "$CURRY2GOHOME/tools/rlwrap.words" "$REPL" --using "$CPMVERSION" ${1+"$@"}
else
  "$REPL" --using "$CPMVERSION" ${1+"$@"}
fi
