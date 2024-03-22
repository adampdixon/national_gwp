library(LWFBrook90R)
library(geosphere)

# dates <- seq.Date(as.Date("1850-01-01"), as.Date("2021-12-31"), by = 'day')
# calc_globrad(dates, sunhours = runif(365, 0, 7),lat = 52.8)
# 
# daylength(lat = 52.8, doy = 1)
# 
# f<-calc_globrad(dates, sunhours = runif(length(dates), 0, 7),lat = 52.8, full_output = TRUE)
# tail(f)


data<-read.csv('/home/ap/Documents/GitHub/national_gwp/Data/County_start/county_centroids_elevation_crops_global_rad.csv')
head(data)

# length_day<-data.frame()
# get day length
# for (c in 1:nrow(data)){
#   for(d in 1:365){
#     length_day<-rbind(length_day, data.frame(GEOID =  data[c,]$GEOID, doy = d, daylength_h = daylength(lat = data[c,]$Lat, doy = d)))
#     }
# }


d<-data[1,]
length_day<-data.frame()
for(day in 1:365){
  length_day<-rbind(length_day, data.frame(GEOID =  d$GEOID, doy = day, daylength_h = daylength(lat = d$Lat, doy = day)))
  }

# calc global radiation

sh<-length_day$daylength_h
dates <- seq.Date(as.Date("1850-01-01"), as.Date("1850-12-31"), by = 'day')
f<-calc_globrad(dates, sunhours = sh,lat = d$Lat, full_output = TRUE)
tail(f)
barplot(f$globrad)

dates <- seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by = 'day')
g<-calc_globrad(dates, sunhours = sh,lat = d$Lat, full_output = TRUE)
tail(g)
barplot(g$globrad)
