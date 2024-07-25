#!/bin/bash
#PBS -N Casper_1
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/Casper_1.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/Casper_1.stderr 
#PBS -l select=1:ncpus=1:mem=5GB
#PBS -l walltime=00:10:00
#PBS -q casper
#PBS -J 3050-3060
#PBS -j oe
#PBS -r y


#conda activate casper_2023
ml ncarenv/23.10 udunits conda
conda activate r-4.3
  
#conda activate casper_2023
echo "Hello World from $PBS_ARRAY_INDEX"            
CMD="Rscript /glade/derecho/scratch/apdixon/national_gwp/000_County_Run.R --args $PBS_ARRAY_INDEX >& $PBS_ARRAY_INDEX.log &"
echo $CMD
$CMD
