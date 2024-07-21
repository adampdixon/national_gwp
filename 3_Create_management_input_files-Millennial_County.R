#######################################
# Script: 3_Create_management_input_files-Millennial2.R
# Author: Ellen Maas and then Adam Dixon
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: Creates input files with soil temperature, moisture, 
# and daily C input as calculated via Daycent and LDNDC.
#######################################
# Audit Log
# 9/23/2022: Created script.
#######################################

print("Starting 3_Create_management_input_files-Millennial.R")

#**********************************************************************
# AD Brought over from 3_Create_management_input_files-setupRM2

# Daycent Carbon is 0 to 25 cm
# Millenium is to 1 m
# LDNDC is to 20 cm


# local constants

## keep these: set internally:

mill_scenario_filename<-paste0("siteenviron_", scenario_name2,"_in.txt")

mill_init_filename <- "globalaverage.txt"
mill_equilinit_filename <- paste0(mill_path,paste0('siteenviron_eq_', crop, '_in.txt'))
# mill_corninit_filename <- "corn_init.txt"
# mill_soybeaninit_filename <- "soy_init.txt"
# mill_wheatinit_filename <- "wheat_init.txt"
# mill_eqilinput_filename <- "siteenviron_eq_in.txt"
mill_baseinput_filename <- paste0(mill_path, mill_scenario_filename)

# Daycent soilc -> C in soil organic matter pools (g C m-2)
# NPP – Summation of all production values (g C m-2 d-1)
Daycent_daily_data<-fread(daycent_daily_out)%>%
  mutate(daily_soilC_gm2_daycent = SOC_Mgha,
         dayofyr = dayofyear,
         month = month(date),
         water_content_daycent = vswc_pct_layer4,
         soil_temp_daycent = soil_tavg_layer4,
         NPP_daycent = ifelse(is.na(NPP), 0, NPP))%>%
  select(year, date, dayofyear, dayofyr, month, NPP_daycent, water_content_daycent, soil_temp_daycent, daily_soilC_gm2_daycent)

LDNDC_daily_data<-fread(ldndc_daily_out)%>%
  mutate(daily_soilC_gm2_ldndc = SOC_Mgha,
         dayofyr = dayofyear,
         month = month(datetime),
         water_content_ldndc = soilwater_20cm,
         soil_temp_ldndc = s_temp_20cm,
         NPP_ldndc = ifelse(is.na(NPP_gCm2), 0, NPP_gCm2))%>%
  select(year, dayofyear, NPP_ldndc, water_content_ldndc, soil_temp_ldndc, daily_soilC_gm2_ldndc)

summary(Daycent_daily_data$daily_soilC_gm2)

summary(LDNDC_daily_data$daily_soilC_gm2)

# Summarize LDNDC and Daycent by averaging values

daily_data<-left_join(Daycent_daily_data, LDNDC_daily_data, by = c('year', 'dayofyear'))%>%
  mutate(daily_soilC_gm2 = (daily_soilC_gm2_daycent + daily_soilC_gm2_ldndc)/2,
         water_content = ((water_content_daycent + water_content_ldndc)/2)/100, # convert to fraction percent
         soil_temp = (soil_temp_daycent + soil_temp_ldndc)/2,
         NPP = (NPP_daycent + NPP_ldndc)/2)%>%
  select(year, dayofyear, dayofyr, month, date, NPP, daily_soilC_gm2, water_content, soil_temp)



###########################
# For Millennial, daily. Keep in mind that C input from Daycent is for 20 cm.

#**********************************************************************

data_in<-daily_data%>%
  mutate(forc_st = soil_temp,
         forc_sw = water_content,
         forc_npp = NPP,
         month = month(date),
         day = day(date),
         year = year(date),
         crop = crop)

base_data_in<-data_in%>%
  # filter(year>2021)%>%
  select(forc_st, forc_sw, forc_npp, date, crop)%>%
  na.locf() # fill in missing values with last observation (issue on 12-31-2050)


write.table(base_data_in, file=mill_baseinput_filename,
            row.names=F,col.names = T,append=F)



## equilibrium

#Because the Millennial model does not use climate data, and soil temperature and
#moisture data at the site are not available daily, the 1-year global average data supplied
#with the model is used for equilibrium and the base period (pre-experimental).


# create one year's worth of input data for spin-up
eq_inputdata_1 <- read.table(paste0(mill_path,"../",mill_init_filename))
# create equilibrium from 1850-1900 data
eq_inputdata<-data_in%>%
  filter(year<=1900, dayofyr<366)%>%
  select(forc_st, forc_sw, forc_npp, dayofyear)%>%
  group_by(dayofyear)%>%
  summarize_all(mean)%>%
  mutate(crop = crop)%>% # add crop back
  select(forc_st, forc_sw, forc_npp, crop)


# decrease soil temp since air temps on average were lower in 1850
# This was holdover from Ellen's project, should we keep? # Ask Debjani
eq_inputdata[1] <- eq_inputdata[1]-0.5
eq_inputdata_1[1] <- eq_inputdata_1[1]-0.5


# how much total C are we adding, to 20 cm?
eq_annual_C <- sum(eq_inputdata_1[3])

write.table(eq_inputdata_1, file=mill_equilinit_filename,
            row.names=F,col.names = T,append=F)


print(paste0("Millenial base data written to file:   ", mill_baseinput_filename))
print(paste0("Millenial equilibrium data written to file:   ", mill_equilinit_filename))
