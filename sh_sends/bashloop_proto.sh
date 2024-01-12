#!/bin/bash

echo "Running 000_County_Run.R in bash parallel"

# Define the task to be executed in parallel
task () {
  Rscript 000_County_Run.R --args $1
  sleep 0.5
  echo "$1"
}

# Define the number of parallel processes
NUM_PARALLEL=3

# Read the input arguments from a file and run the command in parallel
for i in $(seq 1 3); do
  echo "Starting task $i"
  task $i &
  if (( $i % $NUM_PARALLEL == 0 )); then
    wait
  fi
done

