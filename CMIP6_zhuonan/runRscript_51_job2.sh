#!/bin/bash 
########################
### Job name
#PBS -N CM49_56
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_49_56.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_49_56.stderr 
### Queue name
#PBS -q casper
##PBS -q regular 
#PBS -l walltime=12:00:00
### Number of nodes
#PBS -l select=1:ncpus=36:mem=109GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_51_job2.R


