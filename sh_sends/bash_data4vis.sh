#!/bin/bash 
########################
### Job name
#PBS -N data4vis_processing
#PBS -A UEMO0003
#PBS -o /glade/derecho/scratch/apdixon/national_gwp/sh_sends/data4vis.stdout
#PBS -e /glade/derecho/scratch/apdixon/national_gwp/sh_sends/data4vis.stderr 
### Queue name
#PBS -q main
#PBS -l walltime=05:00:00
### Send email on abort, begin and end
#PBS -m e
### Specify mail recipient
#PBS -M apdixon@pm.me
### Number of nodes
#PBS -l select=1:ncpus=1:mem=1GB
###PBS -l select=1:ncpus=32:mem=235GB
########################


### Use scratch for temporary files to avoid space limits in /tmp
###export TMPDIR=/glade/scratch/apdixon/temp
###mkdir -p $TMPDIR

#!/bin/bash

echo "Running data4vis"


Rscript /glade/derecho/scratch/apdixon/national_gwp/9_Results_Input_Data_4Vis_County.R
  