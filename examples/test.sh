#!/bin/sh
# Shell script to test some examples

CURRYHOME=$(dirname $(dirname $(realpath $0)))
CURRYBIN=$CURRYHOME/bin

PATH=$CURRYBIN:$PATH
export PATH

CURRYPATH=$CURRYHOME/lib
export CURRYPATH

C2GO=curry2goc

# Clean old stuff:
clean() {
  /bin/rm -rf .curry .gocurry
  for P in $PROGRAMS ; do
    /bin/rm -f $P
  done
}

run() {
  for P in $PROGRAMS ; do
    echo "Executing: $P"
    $C2GO $CGOPTS $P
  done
}

testall() {
  TESTRESULT=$1
  echo "TESTING ALL PROGRAMS WITH OPTIONS: $CGOPTS"
  LOGFILE=XXX$$
  clean
  run | tee $LOGFILE
  clean
  
  # Check differences:
  DIFF=diff$$
  diff $TESTRESULT $LOGFILE > $DIFF
  if [ "`cat $DIFF`" = "" ] ; then
    echo
    echo "REGRESSION TEST SUCCESSFULLY EXECUTED!"
    /bin/rm -f $LOGFILETEE $LOGFILE $DIFF
  else
    echo
    echo "DIFFERENCES IN REGRESSION TEST OCCURRED:"
    cat $DIFF
    /bin/rm -f $DIFF $LOGFILETEE
    /bin/mv -f $LOGFILE LOGFILE
    echo "Test output saved in file 'LOGFILE'."
    exit 1
  fi
}

# Tests where strategy is not relevant:
PROGRAMS="CaseLiteral Fac FreeBool Higher Last InfList PermSort PermSortInt Rev Xor Zip"
CGOPTS="-q -r --dfs"
testall TESTANYSTRAT.txt
CGOPTS="-q -r --bfs"
testall TESTANYSTRAT.txt
CGOPTS="-q -r --fs"
testall TESTANYSTRAT.txt

# Test with DFS strategy (to check fixed order of results):
PROGRAMS="Colormap ColormapFree Data Half NonDet Perm PullTabOwnerTask"
CGOPTS="-q -r --dfs"
testall TESTDFS.txt

# Tests where BFS strategy is relevant:
PROGRAMS="NDNums Strategy"
CGOPTS="-q -r --bfs --first"
testall TESTBFS.txt

# Tests where fair strategy is relevant:
PROGRAMS="FairSearch"
CGOPTS="-q -r --fs --first"
testall TESTFS.txt

# Tests with functional patterns:
PROGRAMS="Dutch FunPatsLast FunPatsPali FunPatsExpSimp FunPatsExpVar"
CGOPTS="-q -r --fs"
testall TESTFUNPATS.txt
CGOPTS="-q -r --dfs"
testall TESTFUNPATS.txt
CGOPTS="-q -r --bfs"
testall TESTFUNPATS.txt
