#!/bin/bash 
########################
### Job name
#PBS -N CM160
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_161.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_161.stderr 
### Queue name
###PBS -q casper
#PBS -q regular
#PBS -l walltime=12:00:00
### Number of nodes
#PBS -l select=1:ncpus=11:mem=109GB
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M zhuonan.wang@emory.edu
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_160_Cheyenne.R


