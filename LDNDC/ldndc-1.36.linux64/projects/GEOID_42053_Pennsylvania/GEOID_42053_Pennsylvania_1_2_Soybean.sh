#!/bin/bash

if [ -e $HOME/.ldndc ]; 
then
#LandscapeDNDC program
ldndc="../../bin/ldndc --create-checkpoint"

#Target project
project="./GEOID_42053_Pennsylvania_1_2_Soybean.ldndc"

#Run target project
$ldndc $project
else
printf "Directory \"~/.ldndc\" missing!
\
Did you install LandscapeDNDC via \"install.sh\"?
"
fi
