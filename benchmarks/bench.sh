#!/bin/bash

#Usage: ./bench.sh [strat]
#       strat is the search strategy used when executing a benchmark
#       and can be one of: dfs, bfs, fs. Default is fs.

# Number of iterations:
ITER=3
# Output in CSV format?
CSV=yes

#parse command line arguments
case "$1" in
    "dfs") STRAT=""
        ;;
    "bfs") STRAT="--bfs"
        ;;
    "fs") STRAT="--fs"
        ;;
    "") ;;
    *) echo "Invalid argument!"; exit;
        ;;
esac

#delete old files to ensure new compilation
rm -rf .curry

#run all benchmarks in the directory
run_benchmarks() {
  STRAT=$1
  if [ $CSV = yes ] ; then
    echo Program,$STRAT
  else
    echo Strategy: $STRAT
  fi
  for FILE in *
  do
    #only compile curry files
    if [[ $FILE == *".curry" ]]
      then
        FILE=${FILE%".curry"}

        if [ $CSV = yes ] ; then
          echo -n "$FILE,"
        else
          echo "Running benchmark: $FILE"
        fi
        RESULT=$(curry2go --time=$ITER -r $STRAT $FILE | tail -1)
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
