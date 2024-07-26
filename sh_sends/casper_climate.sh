#!/bin/bash
#PBS -N Casper_clim
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/Casper_clim.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/Casper_clim.stderr 
#PBS -l select=1:ncpus=1:mem=1GB
#PBS -l walltime=00:03:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
#PBS -q casper
#PBS -J 1-24
#PBS -j oe
#PBS -r y


#conda activate casper_2023
ml ncarenv/23.10 udunits conda
conda activate r-4.3
  
#conda activate casper_2023
echo "Hello World from $PBS_ARRAY_INDEX"            
CMD="Rscript /glade/derecho/scratch/apdixon/national_gwp/Utilities/1_create_county_climate_input_AD_v4_.R --args $PBS_ARRAY_INDEX >& $PBS_ARRAY_INDEX.log &"
echo $CMD
$CMD
