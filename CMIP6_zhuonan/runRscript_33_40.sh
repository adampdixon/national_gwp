#!/bin/bash 
########################
### Job name
#PBS -N CM33_40
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_33_40.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_33_40.stderr 
### Queue name
#PBS -q casper
#PBS -l walltime=24:00:00
### Number of nodes
#PBS -l select=1:ncpus=36:mem=200GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_33_40.R


