#!/bin/bash
#PBS -N All_casper_1
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/All_casper_1.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/All_casper_1.stderr 
#PBS -l select=1:ncpus=1:mem=4GB
#PBS -l walltime=00:01:00
#PBS -q casper
#PBS -J 1-10
#PBS -j oe
#PBS -r y

ml load ncarenv-basic/23.09 udunits conda
conda activate r-4.3
  
#conda activate casper_2023
echo "Hello World from $PBS_ARRAY_INDEX"            
CMD="Rscript /glade/derecho/scratch/apdixon/national_gwp/sh_sends/casper_bash_proto.R --args $PBS_ARRAY_INDEX >& $PBS_ARRAY_INDEX.log &"
echo $CMD  
