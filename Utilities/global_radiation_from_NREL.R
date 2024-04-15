# get global horizontal radiation per county for month
# data will be extended to daily in script for each county using monthly daily average for each day

# this uses average value or each month

# global radiation data downloaded from
# https://www.nrel.gov/gis/solar-resource-maps.html

library(lubridate)
library(terra)
library(sf)
library(dplyr)


data<-list.files('/home/ap/global_radiation/nsrdbv3_ghi/Monthly GHI', full.names = T, pattern = 'tif')
data


# jan = 1:31
# feb = 32:59
# mar = 60:90
# apr = 91:120
# may = 121:151
# jun = 152:181
# jul = 182:212
# aug = 213:243
# sep = 244:273
# oct = 274:304
# nov = 305:334
# dec = 335:365

# months = c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

month<-c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

counties_shp<-vect(st_read('/home/ap/Documents/GitHub/national_gwp/Data/County_start/county_shp/ne_conus_counties.shp')%>%st_transform(5070))

counties_rast<-rasterize(counties_shp, month_sol, field = 'CODE_LOCAL')

df<-data.frame(GEOID=unique(counties_rast))
for (m in month){
  print(m)
  month_sol<-project(rast(data[grep(m, data)]), crs(counties_shp))
  counties_solar<-zonal(month_sol, counties_rast, fun = 'mean')
  df<-cbind(df, counties_solar[,2])
}


names(df)<-c('GEOID','jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
df

# create a day of year table
monthly_<-data.frame()
counter<-0
for(g in df$GEOID){
  counter<-counter+1
  print(paste(g, " ", counter))
  for(m in month){
    # print(m)
    # print(df[df$GEOID==g, m])
    monthly_<-rbind(monthly_, data.frame(GEOID=g, month=m, solar_glob_rad_Wm2=df[df$GEOID==g, m]))
  }

}

head(monthly_)

write.csv(monthly_, '/home/ap/Documents/GitHub/national_gwp/Data/County_start/solar_glob_rad.csv')

# month_sol<-project(rast(data[grep('jan', data)]), crs(counties_shp))
# 
# counties_solar<-zonal(month_sol, counties_rast, fun = 'mean')
