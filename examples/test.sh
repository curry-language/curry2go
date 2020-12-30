#!/bin/sh
# Shell script to test some examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

C2GO=curry2go

# Clean old stuff:
clean() {
  /bin/rm -rf .curry .gocurry
  for P in $PROGRAMS ; do
    /bin/rm -f $P
  done
}

run() {
  for P in $PROGRAMS ; do
    echo "Running: $C2GO $CGOPTS $P"
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

# Tests
PROGRAMS="Colormap ColormapFree Data Fac FreeBool Half Higher Last InfList NonDet Perm PermSort Rev Xor Zip"
# currently not working: CaseLiteral
CGOPTS="-r"
testall TESTDFS.txt

# Tests where BFS strategy is relevant:
PROGRAMS="NDNums Strategy"
CGOPTS="-r --bfs --first"
testall TESTBFS.txt

# Tests where fair strategy is relevant:
PROGRAMS="FairSearch"
CGOPTS="-r --fs --first"
testall TESTFS.txt

exit

# Tests with functional patterns:
PROGRAMS="FunPatsLast FunPatsPali FunPatsExpSimp FunPatsExpVar"
CGOPTS="-r"
testall TESTFUNPATS.txt
