#!/bin/bash 
########################
### Job name
#PBS -N All_models
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/day_.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/day_.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=8:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=50:mem=70GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/scratch/apdixon/temp
###mkdir -p $TMPDIR

#!/bin/bash

echo "Running 000_County_Run.R in bash parallel"

# Define the task to be executed in parallel
task () {
  ml load ncarenv-basic/23.09 gcc gdal geos proj udunits conda
  conda activate r-4.3
  Rscript /glade/derecho/scratch/apdixon/national_gwp/000_County_Run.R --args $1
  sleep 0.5
  echo "$1"
}

# Define the number of parallel processes
NUM_PARALLEL=50

# Read the input arguments from a file and run the command in parallel
for i in $(seq 726 826); do
  echo "Starting task $i"
  task $i &
  if (( $i % $NUM_PARALLEL == 0 )); then
    wait
  fi
done
# 
