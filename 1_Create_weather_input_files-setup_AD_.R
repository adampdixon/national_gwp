setwd('/glade/work/apdixon')

nclim_dir<-'/glade/work/apdixon/Output_nClimGrid'

fields<-c('GEOID', 'date', 'precip','tmax','tmin','doy')

precip_raw<-list.files(nclim_dir, pattern = 'precip')

print(precip_raw)