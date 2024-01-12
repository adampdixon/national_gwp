#!/bin/bash

echo "Running 000_County_Run.R in bash parallel"

# Define the task to be executed in parallel
task () {
  ml load ncarenv-basic/23.09 gcc gdal geos proj udunits conda rstudio
  conda activate r-4.3
  Rscript /glade/derecho/scratch/apdixon/national_gwp/bashloop_prototype_.R --args $1
  sleep 0.5
  echo "$1"
}
sh
# Define the number of parallel processes
NUM_PARALLEL=1

# Read the input arguments from a file and run the command in parallel
for i in $(seq 1 10); do
  echo "Starting task $i"
  task $i &
  if (( $i % $NUM_PARALLEL == 0 )); then
    wait
  fi
done
