#!/bin/bash

if [ -e $HOME/.ldndc ]; 
then
#LandscapeDNDC program
ldndc="../../bin/ldndc --create-checkpoint"

#Target project
project="./GEOID_13023_Georgia_1_4_Soybean.ldndc"

#Run target project
$ldndc $project
else
printf "Directory \"~/.ldndc\" missing!
\
Did you install LandscapeDNDC via \"install.sh\"?
"
fi
