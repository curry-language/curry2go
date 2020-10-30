#!/bin/bash

#Usage: ./bench.sh [strat]
#       strat is the search strategy used when executing a benchmark
#       and can be one of: dfs, bfs, fs. Default is fs.

strat="--fs"

#parse command line arguments
case "$1" in
    "dfs") strat=""
        ;;
    "bfs") strat="--bfs"
        ;;
    "fs") strat="--fs"
        ;;
    "") ;;
    *) echo "Invalid argument!"; exit;
        ;;
esac

#delete old files to ensure new compilation
if [ -d ".gocurry" ]
  then
    rm -r .gocurry
fi
exit

#run all benchmarks in the directory
for file in *
do
    #only compile curry files
    if [[ $file == *".curry" ]]
      then
        file=${file%".curry"}

        echo Running benchmark: $file        
        result=$(curry2go --time=10 -r $strat $file | tail -1)
        echo $result
    fi 
done


