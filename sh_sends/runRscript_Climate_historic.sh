#!/bin/bash 
########################
### Job name
#PBS -N Climate historic
#PBS -A UEMO0003
#PBS -o /Documents/GitHub/national_gwp/sh_sends/job_climate.stdout
#PBS -e /Documents/GitHub/national_gwp/sh_sends/job_climate.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=12:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=2:ompthreads=1:mem=100GB
###PBS -l select=1:ncpus=32:mem=235GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/derecho/scratch/$zhuonan/temp
###mkdir -p $TMPDIR

module restore R_processing

Rscript /glade/u/home/apdixon/Documents/national_gwp/1_Create_weather_input_files-setup_AD_.R
# 

