#!/bin/bash 
########################
### Job name
#PBS -N CM52_72
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_52_72.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_52_72.stderr 
### Queue name
#PBS -q casper
#PBS -l walltime=24:00:00
### Number of nodes
#PBS -l select=1:ncpus=36:mem=200GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_52_72.R


