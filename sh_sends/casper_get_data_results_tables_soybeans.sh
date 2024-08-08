#!/bin/bash
#PBS -N run_get_model_tables
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/run_get_model_tables.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/run_get_model_tables.stderr 
#PBS -l select=1:ncpus=1:mem=10GB
#PBS -l walltime=03:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
#PBS -q casper
#PBS -j oe
#PBS -r y

### #PBS -J 1-5 so that it indexes 5 crops

#conda activate casper_2023
ml ncarenv/23.10 udunits conda
conda activate r-4.3
  
#conda activate casper_2023
echo "Hello World run_get_model_tables from soybeans"         
CMD="Rscript /glade/derecho/scratch/apdixon/national_gwp/data_explore/run_get_model_tables.R --args 2"
echo $CMD
$CMD
