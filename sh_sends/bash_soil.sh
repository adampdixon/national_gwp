#!/bin/bash 
########################
### Job name
#PBS -N NATSGO_processing
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/soils.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/soils.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=4:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=1:mem=1GB
###PBS -l select=1:ncpus=32:mem=235GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/scratch/apdixon/temp
###mkdir -p $TMPDIR

#!/bin/bash

echo "Running 2_Create_soil_data_input_files_gNATSGO.R in bash parallel"

# Define the task to be executed in parallel
task () {
  ml load ncarenv-basic/23.09 gcc gdal geos proj udunits conda
  conda activate r-4.3
  Rscript /glade/derecho/scratch/apdixon/national_gwp/2_Create_soil_data_input_files_gNATSGO.R --args $1
  sleep 0.5
  echo "$1"
}

# Define the number of parallel processes
NUM_PARALLEL=1

# Read the input arguments from a file and run the command in parallel
for i in $(seq 0 3108); do
  echo "Starting task $i"
  task $i &
  if (( $i % $NUM_PARALLEL == 0 )); then
    wait
  fi
done
# 
