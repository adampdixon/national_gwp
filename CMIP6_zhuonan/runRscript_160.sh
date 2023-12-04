#!/bin/bash 
########################
### Job name
#PBS -N CM160
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_160.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_160.stderr 
### Queue name
#PBS -q casper
#PBS -l walltime=24:00:00
### Number of nodes
#PBS -l select=1:ncpus=36:mem=300GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_160.R


