#!/bin/bash 
########################
### Job name
#PBS -N Climate_data
#PBS -A UEMO0003
#PBS -o /glade/u/home/apdixon/Documents/national_gwp/sh_sends/job_climate.stdout
#PBS -e /glade/u/home/apdixon/Documents/national_gwp/sh_sends/job_climate.stderr 
### Queue name
#PBS -q regular
#PBS -l walltime=6:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=7:ompthreads=7:mem=64GB
###PBS -l select=1:ncpus=32:mem=235GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/scratch/apdixon/temp
###mkdir -p $TMPDIR

module restore R_processing

Rscript /glade/u/home/apdixon/Documents/national_gwp/1_Create_weather_input_files-setup_AD_.R
# 

