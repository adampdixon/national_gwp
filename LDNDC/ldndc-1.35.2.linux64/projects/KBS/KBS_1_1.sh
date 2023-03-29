#!/bin/bash

if [ -e $HOME/.ldndc ]; 
then
#LandscapeDNDC program
ldndc="../../bin/ldndc"

#Target project
project="./KBS_1_1.ldndc"

#Run target project
$ldndc $project
else
printf "Directory \"~/.ldndc\" missing!
\
Did you install LandscapeDNDC via \"install.sh\"?
"
fi
