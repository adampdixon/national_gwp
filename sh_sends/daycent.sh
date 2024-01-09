#!/bin/bash 
########################
### Job name
#PBS -N daycent_processing
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/day.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/day.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=12:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=10:mem=20GB
###PBS -l select=1:ncpus=32:mem=235GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/scratch/apdixon/temp
###mkdir -p $TMPDIR

ml load ncarenv-basic/23.09 gcc gdal geos proj udunits conda
conda activate r-4.3

Rscript /glade/derecho/scratch/apdixon/national_gwp/000_County_Run_parallel.R
# 

