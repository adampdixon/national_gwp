#!/bin/bash
#PBS -N Casper_4
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/Casper_4.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/Casper_4.stderr 
#PBS -l select=1:ncpus=1:mem=12GB
#PBS -l walltime=06:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
#PBS -q casper
#PBS -J 1-188
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
