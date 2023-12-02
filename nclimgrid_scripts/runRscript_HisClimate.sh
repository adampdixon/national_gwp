#!/bin/bash 
########################
### Job name
#PBS -N hisclm
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_nClimGrid/job_1.stdout 
#PBS -e /glade/work/zhuonan/Output_nClimGrid/job_1.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=12:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M zhuonan.wang@emory.edu
### Number of nodes
#PBS -l select=1:ncpus=30:ompthreads=30
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/derecho/scratch/$zhuonan/temp
###mkdir -p $TMPDIR

module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/nclimgrid_scripts/HisClimate_data.R


