#!/bin/bash 
########################
### Job name
#PBS -N cdl_county_20
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/day_.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/day_.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=1:30:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=20:mem=20
###PBS -l select=1:ncpus=32:mem=235GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/scratch/apdixon/temp
###mkdir -p $TMPDIR

#!/bin/bash

echo "running CDL parallel funcion"

ml load ncarenv-basic/23.09 gcc gdal geos proj udunits conda
conda activate r-4.3

Rscript /glade/derecho/scratch/apdixon/national_gwp/data_explore/cdl_extract_county_crops_from_map.R
