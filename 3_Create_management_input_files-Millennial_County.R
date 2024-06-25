#######################################
# Script: 3_Create_management_input_files-Millennial2.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: Creates input files with soil temperature, moisture, 
# and daily C input as calculated via Daycent.
#######################################
# Audit Log
# 9/23/2022: Created script.
#######################################

print("Starting 3_Create_management_input_files-Millennial2.R")

# 
# library(readxl)
# library(dplyr)
# library(lubridate)
# library(tidyverse)
# library(data.table)
# library(XML)
# library(stringr)

#**********************************************************************
# AD Brought over from 3_Create_management_input_files-setupRM2

# Daycent Carbon is 0 to 25 cm
# Millenium is to 1 m
# LDNDC is to 20 cm


# local constants

## keep these: set internally:

# mgmt_path <- paste0("Data/",site_name,"/Management/")

mill_scenario_filename<-paste0("siteenviron_", scenario_name2,"_in.txt")

mill_init_filename <- "globalaverage.txt"
mill_equilinit_filename <- paste0(mill_path,paste0('siteenviron_eq_', crop, '_in.txt'))
# mill_corninit_filename <- "corn_init.txt"
# mill_soybeaninit_filename <- "soy_init.txt"
# mill_wheatinit_filename <- "wheat_init.txt"
# mill_eqilinput_filename <- "siteenviron_eq_in.txt"
mill_baseinput_filename <- paste0(mill_path, mill_scenario_filename)



# mill_futinput_filename <- "siteenviron_in"

# soil_temp_bias <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4,5), 5,
#                           if_else(mgmt_scenario_num==2, 4.5,
#                                   0))
# soil_moist_bias <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4,5), 2,
#                            if_else(mgmt_scenario_num==2, 0,
#                                    0))

# Daycent soilc -> C in soil organic matter pools (g C m-2)
# NPP – Summation of all production values (g C m-2 d-1)
Daycent_daily_data<-fread(file.path(results_path, paste0("Daily_results_compilation_", scenario_name2,"_Daycent.csv")))%>%
  mutate(daily_soilC_gm2_daycent = soilc,
         dayofyr = dayofyear,
         month = month(date),
         water_content_daycent = vswc_pct_layer4,
         soil_temp_daycent = soil_tavg_layer4,
         NPP_daycent = ifelse(is.na(NPP), 0, NPP))%>%
  select(year, date, dayofyear, dayofyr, month, NPP_daycent, water_content_daycent, soil_temp_daycent, daily_soilC_gm2_daycent)

LDNDC_daily_data<-fread(file.path(results_path, paste0("Daily_results_compilation_", scenario_name2,"_LDNDC.csv")))%>%
  mutate(daily_soilC_gm2_ldndc = `SOC_40cm[gm2-1]`,
         dayofyr = dayofyear,
         month = month(date),
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
# Cin_daily_gm2 <- daily_data[,c("date","year","dayofyr","month","daily_soilC_gm2")] %>%
#   mutate(day=day(date))


###########################################
# Soil temperature, for Millennial
###########################################

# Daycent data, experimental period through future
## soil temperature is reduced to reduce warm bias
# Tin_daily_C <-   merge(ObsTemp,
#                        DayT_C_all[,c("date","year","mean_3_4")],
#                        by=c("date","year"),
#                        all=TRUE) %>%
#   mutate(soil_T=ifelse(is.na(soil_temperature),mean_3_4,soil_temperature))

# Tin_daily_C <- daily_data[,c("date","year","dayofyr","month","soil_temp")] %>%
#   mutate(day=day(date), soil_T = soil_temp)


###########################################
# Soil moisture, for Millennial
###########################################

## Use observations when available; fill in with Daycent estimates

# Min_daily_V <- merge(ObsVSM[,c("date","year","mean_VSM")],
#                      DayM_V_all[,c("date","year","layer4_pct")],
#                      by=c("date","year"),
#                      all=TRUE) %>%
#   mutate(soil_M=ifelse(is.na(mean_VSM),layer4_pct,mean_VSM)/100)

# Min_daily_V <- daily_data[,c("date","year","dayofyr","month","water_content")] %>%
#   mutate(day=day(date), soil_M=water_content/100)


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
eq_inputdata[1] <- eq_inputdata[1]-0.5
eq_inputdata_1[1] <- eq_inputdata_1[1]-0.5


# AD - not doing the below
# Using the daily percentages in the equilibrium initialization file, assign
# g C/m^2 per day of the annual total determined by RothC inverse mode to reach
# equilibrium at this site
# equil_C_input_adj <- equil_C_input # try adjusting the C input for calibration
# equil_init <- read.table(paste0(mill_path,"../",mill_equilinit_filename),header=T)
# eq_inputdata[3] <- round(equil_init*equil_C_input_adj,5)
# eq_inputdata[,4:5] <- Cin_daily_gm2[1:365,c("month","day")] # add date elements to help with readability
# names(eq_inputdata) <- c("forc_st","forc_sw","forc_npp","month","day")

# how much total C are we adding, to 20 cm?
eq_annual_C <- sum(eq_inputdata_1[3])
#




write.table(eq_inputdata_1, file=mill_equilinit_filename,
            row.names=F,col.names = T,append=F)


print(paste0("Millenial base data written to file:   ", mill_baseinput_filename))
print(paste0("Millenial equilibrium data written to file:   ", mill_equilinit_filename))

## base-experimental

# Reuse global average data for each year in the base period up to the experimental
# period for soil temperature and moisture. Annual C input comes from the 
# historical estimates calculated via Bolinder and output from APSIM.
# All years are 365 days.


# start data file for base run with soil temp and moisture from Daycent/LDNDC
# mill_inputdata <- merge(Tin_daily_C[,c("date","soil_T")],
#                         Min_daily_V[,c("date","soil_M")],
#                         by="date") %>%
#   merge (Cin_daily_gm2[,c("date","daily_soilC_gm2")],
#          by="date") %>%
#   mutate(year=year(date))
# 
# 
# write.table(mill_inputdata, file=paste0(mill_path,mill_scenario_filename),
#             row.names=F,col.names = T,append=F)





####################
#################### AD Commenting out the below
# 
# 
# 
# 
# # increase the C input after land conversion as needed to account for the fact
# # that the C input data is coming from Daycent, which is only to 25 cm, but
# # Millennial is to 1 m. This adjustment is set purely through trial and error,
# # to reproduce historical/experimental measurements.
# # soc_adj <- if_else(mgmt_scenario_grp==1,2,
# #                    if_else(mgmt_scenario_grp==2,1.2,1))
# soc_adj <- if_else(mgmt_scenario_grp==3,.95,
#            if_else(mgmt_scenario_grp==2,1.2,2))
# mill_inputdata$daily_soilC_gm2 <- mill_inputdata$daily_soilC_gm2*soc_adj
# base_inputdata = data.frame()
# 
# for(i in 1850:1988) { 
#   if (i == 1850) { # start with fresh file creation; all other write.table calls appends data
#     # need to limit this to 365 records (can't use 366 in leap years)
#     base_inputdata <- head(mill_inputdata[mill_inputdata$year==i,
#                                                c("soil_T","soil_M","daily_soilC_gm2")],365)
#     base_inputdata[,4] <- seq(as.Date(paste0(i,"-01-01")),
#                               length.out=365,by="day")
#     base_inputdata[,5] <- "corn"
#     write.table(base_inputdata, file=paste0(mill_path,mill_baseinput_filename),
#                 col.names=c("forc_st","forc_sw","forc_npp","date","crop"), 
#                 row.names=FALSE, sep="\t", 
#                 quote=FALSE, append=FALSE)
#   } # end 1850 loop initialization
#   if (i >= 1851 & i <= 1953) { # continuous corn
#     base_inputdata[1:3] <- head(mill_inputdata[mill_inputdata$year==i,
#                                                c("soil_T","soil_M","daily_soilC_gm2")],365)
#     base_inputdata[,4] <- seq(as.Date(paste0(i,"-01-01")),
#                               length.out=365,by="day")
#     base_inputdata[,5] <- "corn"
#     write.table(base_inputdata, file=paste0(mill_path,mill_baseinput_filename),
#                 col.names=FALSE, row.names=FALSE, sep="\t", 
#                 quote=FALSE, append=TRUE)
#   } #end 1850-1953
#   else   if (i >= 1954 & i < experiment_start_year) { # landman alternates 2 years: soybeans and corn, but C distribution is same
#     base_inputdata[1:3] <- head(mill_inputdata[mill_inputdata$year==i,
#                                                c("soil_T","soil_M","daily_soilC_gm2")],365)
#     base_inputdata[,4] <- seq(as.Date(paste0(i,"-01-01")),
#                               length.out=365,by="day")
#     base_inputdata[,5] <- if ((i %% 2) == 0) "soybeans" else "corn"
#     write.table(base_inputdata, file=paste0(mill_path,mill_baseinput_filename),
#                 col.names=FALSE, row.names=FALSE, sep="\t", 
#                 quote=FALSE, append=TRUE)
#   } #end 1954-one year before start year
# } #end 1850-1988
# 
# ##########################################
# ####### start experimental period ########
# ##########################################
# 
# exp_inputdata <- mill_inputdata[mill_inputdata$year >= experiment_start_year &
#                                   mill_inputdata$year <= experiment_end_year,
#                                 c("date","soil_T","soil_M","daily_soilC_gm2")] %>% 
#   mutate(year=year(date),
#          crop=if_else(year >= 1989 & year <= 1994 & (year %% 2)==0, "soybeans",
#               if_else(year >= 1989 & year <= 1994 & (year %% 2)==1, "corn",
#               if_else(year >= 1995 & year <= 2021 & (year %% 3)==0, "wheat",
#               if_else(year >= 1995 & year <= end_fut_period_year & (year %% 3)==1, "corn",
#                       "soybeans"))))
#   )
# 
# #cover crop scenario calibration is too high; reducing Cinput
# if(mgmt_scenario_grp==3) {
#   exp_inputdata$daily_soilC_gm2 <- exp_inputdata$daily_soilC_gm2*0.90
# }
# 
# write.table(exp_inputdata[,c("soil_T","soil_M","daily_soilC_gm2","date","crop")], 
#             file=paste0(mill_path,mill_baseinput_filename),
#             col.names=FALSE, row.names=FALSE, sep="\t", 
#             quote=FALSE, append=TRUE)
# 
# 
# 
# ## future
# 
# fut_inputdata <-mill_inputdata[mill_inputdata$year > experiment_end_year,
#                                c("date","soil_T","soil_M","daily_soilC_gm2")]  %>%
#   mutate(year=year(date),
#          crop=if_else((year %% 3)==0, "wheat",
#               if_else((year %% 3)==1, "corn",
#               "soybeans"))
#   )
# 
# #cover crop scenario calibration is too high; reducing Cinput
# if(mgmt_scenario_grp==3) {
#   fut_inputdata$daily_soilC_gm2 <- fut_inputdata$daily_soilC_gm2*0.90
# }
# 
# 
# write.table(fut_inputdata[,c("soil_T","soil_M","daily_soilC_gm2","date","crop")], 
#             file=paste0(mill_path,mill_futinput_filename,
#                         "_",clim_scenario_num,"_",mgmt_scenario_num,".txt"),
#             col.names=c("forc_st","forc_sw","forc_npp","date","crop"),
#             row.names=FALSE, sep="\t", 
#             quote=FALSE, append=FALSE)
#########################
#########################
















# #*******************************************************************
# ### For testing
# #*******************************************************************
# 
#
# plot(mill_inputdata$year,mill_inputdata$soil_T)
# plot(mill_inputdata$year,mill_inputdata$soil_M)
# plot(mill_inputdata$year,mill_inputdata$daily_soilC_gm2)
#
# Cin_daily <- read.csv(file=paste0(mgmt_path,"Daycent_Cinput_",scenario_name,".csv")) %>%
#   mutate(date=as.Date(date))
# plot(Cin_daily$date,Cin_daily$daily_soilC_gm2,data=Cin_daily)
# 
# Tin_daily_C <-   merge(ObsTemp,
#                        DayT_C[,c("date","year","mean_3_4")],
#                        by=c("date","year"),
#                        all=TRUE) %>%
#   mutate(soil_T=ifelse(is.na(soil_temperature),mean_3_4,soil_temperature))
# plot(Tin_daily_C$year,Tin_daily_C$mean_3_4)
# 
# 
# eq_in <- read.delim(file=paste0(mill_path,"siteenviron_eq_in.txt"),sep=" ")
# eq_in$dayofyr <- 1:365
# plot(eq_in$dayofyr,eq_input$forc_npp)
# plot(eq_in$dayofyr,eq_input$forc_sw)
# plot(eq_in$dayofyr,eq_input$forc_st)
# 
# base_in <- read.delim(file=paste0(mill_path,mill_baseinput_filename),sep="\t")
# base_in <- base_in[,1:4]
# base_in$date <- as.Date(base_in$date)
# plot(as.Date(base_in$date),base_in$forc_npp)
# plot(as.Date(base_in$date),base_in$forc_sw)
# plot(as.Date(base_in$date),base_in$forc_st)
# 
# exp_in <- merge(Tin_daily_C[Tin_daily_C$year %in% experiment_year_range,c("date","soil_T")],
#                 Min_daily_V[Min_daily_V$year %in% experiment_year_range,c("date","soil_M")],
#                 by="date") %>%
#   merge (Cin_daily_gm2[Cin_daily_gm2$year %in% experiment_year_range,c("date","daily_soilC_gm2")],
#          by="date")
# exp_in <- exp_in[exp_in$date<"2021-01-01",c(2,3,4,1)]
# exp_in$date <- as.Date(exp_in$date)
# colnames(exp_in) <- c("forc_st","forc_sw","forc_npp","date")
# plot(exp_in$date,exp_in$forc_npp)
# plot(exp_in$date,exp_in$forc_sw)
# plot(exp_in$date,exp_in$forc_st)
# 
# full_in <- rbind(base_in,exp_in)
# full_in <- full_in[full_in$date>="1980-01-01",]
# plot(as.Date(full_in$date),full_in$forc_npp)
# plot(as.Date(full_in$date),full_in$forc_sw)
# plot(as.Date(full_in$date),full_in$forc_st)
# 

