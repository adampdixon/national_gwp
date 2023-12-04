#!/bin/bash 
########################
### Job name
#PBS -N CM17_24
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_17_24.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_17_24.stderr 
### Queue name
#PBS -q casper
#PBS -l walltime=24:00:00
### Number of nodes
#PBS -l select=1:ncpus=30:mem=300GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_17_24.R


