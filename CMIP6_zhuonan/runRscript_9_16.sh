#!/bin/bash 
########################
### Job name
#PBS -N CM9_16
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_9_16.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_9_16.stderr 
### Queue name
#PBS -q casper
#PBS -l walltime=24:00:00
### Number of nodes
#PBS -l select=1:ncpus=30:mem=300GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_9_16.R


