# setwd('/glade/work/apdixon')

nclim_dir<-'/glade/work/apdixon/Output_nClimGrid'

fields<-c('GEOID', 'date', 'precip','tmax','tmin','doy')

precip_raw<-list.files(nclim_dir, pattern = 'prcp', full.names = T)

precip<-precip_raw[grep('.csv', precip_raw)]

#read multiple csvs into data.frame
precip_df<-do.call(rbind, lapply(precip, read.csv))


head(precip_df)

unique(precip_df$GEOID)
# make GEOID, date, doy, precip 

head(read.csv(precip[1]))
unique(read.csv(precip[1])$GEOID)

length(read.csv(precip[1])$GEOID)
