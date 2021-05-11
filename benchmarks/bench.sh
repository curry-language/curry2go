#!/bin/bash
# Simple script to benchmark the Curry2Go compiler

######### BENCHMARKING OPTIONS #########################################
# Number of iterations:
ITER=3
# Output in CSV format?
CSV=yes

########################################################################
ROOT=`pwd`/..

# set CURRYPATH for the compiler:
CURRYPATH=$ROOT/lib
export CURRYPATH

CURRY2GOC=$ROOT/bin/curry2goc

# delete old files to ensure new compilation
#rm -rf .curry

# Run all benchmarks in the directory.
# The argument is the search strategy used when executing a benchmark
# (--dfs, --bfs, --fs).
run_benchmarks() {
  STRAT=$1
  if [ $CSV = yes ] ; then
    echo Program,$STRAT
  else
    echo Strategy: $STRAT
  fi
  for FILE in *
  do
    # compile only Curry files
    if [[ $FILE == *".curry" ]]
      then
        FILE=${FILE%".curry"}

        if [ $CSV = yes ] ; then
          echo -n "$FILE,"
        else
          echo "Running benchmark: $FILE"
        fi
        RESULT=$($CURRY2GOC --time=$ITER -r $STRAT $FILE | tail -1)
        if [ $CSV = yes ] ; then
          RESULT=${RESULT#Average time: }
          RESULT=${RESULT%s}
          echo $RESULT
        else
          echo $RESULT
        fi
        rm $FILE
    fi 
  done
}

# run benchmarks with different strategies:
run_benchmarks --dfs
run_benchmarks --bfs
run_benchmarks --fs
