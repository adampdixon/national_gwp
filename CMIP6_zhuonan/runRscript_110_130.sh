#!/bin/bash 
########################
### Job name
#PBS -N CM110_130
#PBS -A uemo0003
#PBS -o /glade/work/zhuonan/Output_climate/job_110_130.stdout 
#PBS -e /glade/work/zhuonan/Output_climate/job_110_130.stderr 
### Queue name
#PBS -q casper
#PBS -l walltime=24:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M zhuonan.wang@emory.edu
### Number of nodes
#PBS -l select=1:ncpus=36:mem=360GB
########################
module restore default_R
conda activate R_terra

Rscript /glade/work/zhuonan/CMIP6/Climate_data_110_130.R


