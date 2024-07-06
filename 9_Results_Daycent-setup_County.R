# title: "9_Results_Daycent-setup_County.R"
# author: Ellen Maas, adapted by Adam Dixon
# date: 2024-03

print(paste0("Starting 9_Results_Daycent-setup_County.R"))

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)
library(broom)
#test

# tic()

#*********************************************************************
#soil layers to report
nlayers <- 12 # beyond the surface layer, so 13 layers total, calcs below will start with 0 for surface layer
#**********************************************************************

# import Daycent modeled points -------------------------------------------


### most output files (*.out) are limited in time to the specific phase
### they are run from, so they need to be concatenated together in order
### to have the full range of results in one place


################################################################################
## Crop Yield
################################################################################

### harvest - annual output

Day_base_harvest <- fread(file.path(daycent_path2,paste0("harvest_base_",scenario_name2,".csv")))
                             # col_names = TRUE, show_col_types = F)
# Day_exp_harvest <- read_csv(paste0(daycent_path,paste0("harvest_exp_",scenario_name,".csv")),
#                             col_names = TRUE, show_col_types = F)
# Day_fut_harvest <- read_csv(paste0(daycent_path,paste0("harvest_fut_",scenario_name,".csv")),
#                             col_names = TRUE, show_col_types = F)

Day_harvest_raw <- Day_base_harvest %>% # rbind(Day_base_harvest,Day_exp_harvest,Day_fut_harvest) %>%
  mutate(year=floor(time),
         cn_grain_ratio=cgrain/`egrain(N)`,
         cn_stover_ratio=cstraw/`estraw(N)`)

#limit to future scenario time period
Day_harvest <- Day_harvest_raw[Day_harvest_raw$year <= end_fut_period_year,]

# Crop Yield
DayY_Mgha <- Day_harvest[substr(Day_harvest$crpval,2,5)!="CLVC" &
                           substr(Day_harvest$crpval,2,4)!="OAT",] %>%
  select(time,cgrain,crpval) %>%
  mutate(year=floor(time),
         yield=cgrain/100/.45, #g C/m^2 converted to Mg/ha, then divided by .45 to convert C mass to yield mass
         crop=if_else(substr(crpval,2,5)=="C115", "Maize",
                      if_else(substr(crpval,2,5)=="C317", "Maize",
                              if_else(substr(crpval,2,5)=="C418", "Maize",
                                      if_else(substr(crpval,2,5)=="C620", "Maize",
                                              if_else(substr(crpval,2,4)=="COT", "Cotton",
                                                      if_else(substr(crpval,2,2)=="S", "Soybean",
                                                              if_else(substr(crpval,2,2)=="W", "Wheat", "Unknown"))))))))

DayY_Mgha_pivwid <- pivot_wider(DayY_Mgha,names_from="crop",values_from="yield")


################################################################################
## soil temperature - daily
################################################################################

# soil average temperature

Day_base_soiltavg <- fread(file.path(daycent_path2,paste0("soiltavg_base_",scenario_name2,".out")),
                                col.names=c("time","dayofyear", paste0('soil_tavg_layer', 0:nlayers)),
                                colClasses=rep('numeric', nlayers+2+1)) %>% # first two date columns + 2, then nlayers +1 = 13 soil layers
    mutate(year=floor(time),
           date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

################################################################################
## soil moisture - volumetric soil water content
################################################################################
Day_base_vswc <- fread(file.path(daycent_path2,paste0("vswc_base_",scenario_name2,".out")),
                  # widths=c(10,7, rep(10, nlayers+1)), # date, day of year, 13 layers
                  col.names=c("time","dayofyear", paste0('vswc_pct_layer', 0:nlayers)),  # NOTE! will calculated percent in next step. Adding column name here for ease
                  colClasses=rep('numeric', nlayers+2+1))# first two date columns +2, then nlayers +1 = 13 soil layers

DayM_V_all_raw <- Day_base_vswc %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         across(vswc_pct_layer0:eval(paste0('vswc_pct_layer', nlayers)), ~.*100))


DayM_V_all <- DayM_V_all_raw[DayM_V_all_raw$year <= end_fut_period_year,] # this just clips at 2050 in case there are any extra years

###############################################################################
## NPP
###############################################################################
################################################################################
## NPP - Net Primary Productivity (For Millennial)
# mcprd(1) (Column 23) – Daily NPP for shoots for grass/crop system (g C m-2 d-1)
# mcprd(2) (Column 24) – Daily NPP for juvenile roots for grass/crop system (g C m-2 d-1)
# mcprd(3) (Column 25) – Daily NPP for mature roots for grass/crop system (g C m-2 d-1)
# mfprd(1) (Column 26) – Daily NPP for live leaves for tree system (g C m-2 d-1)
# 291May 2, 2018
# mfprd(2) (Column 27) – Daily NPP for live juvenile fine roots for tree system (g C m-2 d-1)
# mfprd(6) (Column 28) – Daily NPP for live mature fine roots for tree system (g C m-2 d-1)
# mfprd(3) (Column 29) – Daily NPP for live fine branches for tree system (g C m-2 d-1)
# mfprd(4) (Column 30) – Daily NPP for live large wood for tree system (g C m-2 d-1)
# mfprd(5) (Column 31) – Daily NPP for live coarse roots for tree system (g C m-2 d-1)
# NPP (Column 32) – Summation of all production values (g C m-2 d-1)

# This is needed to get NPP for Millennial
Day_dc_sip <- fread(file.path(daycent_path2,paste0("dc_sip_base_",scenario_name2,".out")))

Day_dc_sip2 <- Day_dc_sip %>%
  mutate(year=floor(time),
         date=as.Date(dayofyr,origin=paste0(as.character(year),"-01-01"))-1)%>%
  select(year, date, NPP)


Day_NPP <- Day_dc_sip2[Day_dc_sip2$year <= end_fut_period_year,] # this just clips at 2050 in case there are any extra years
  
###############################################################################
## CH4
###############################################################################
# The Daycent summary output file
Day_base_summary <- fread(file.path(daycent_path2,paste0("summary_base_", scenario_name2, ".out")),
                          # widths=c(10,5,9,9,9,13,13,13,13,13),
                          col.names=c("time","dayofyear","tmax","tmin","ppt",
                                      "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"), # TODO Check with Debjani on this. CH4_gChad to CH4_oxid_gChad, N20flux to N20_gNhad
                          skip=1)

Day_methane <- fread(file.path(daycent_path2,paste0("methane_base_",scenario_name2,".out")),
                     # widths=c(4,6,12,12,12,12,12,12,12,12,12,12,12,12,12,
                     #          12,12,12,12,12,12),
                     col.names=c("year","dayofyear","aglivc","bglivcj","bglivcm",
                                 "prev_mcprd1","prev_mcprd2","prev_mcprd3",
                                 "COM","ppt","irri","watr2sat","avgst_10cm",
                                 "TI","Cr","Eh","Feh","CH4_prod","CH4_Ep",
                                 "CH4_Ebl","CH4_oxid")
                     ,skip=1) %>%
  mutate(date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)%>%
  mutate(CH4Emissions_ghaday = CH4_prod*10000) # CH4_prod g C m-2 d-1 to g ha

Day_methane <- Day_methane[Day_methane$year <= end_fut_period_year,]


Day_summary_raw<-Day_base_summary%>%
  mutate(year=floor(time)) %>%
  merge(Day_methane, by=c("year","dayofyear")) %>%
  mutate(CH4_net_gChad = -(CH4_oxid_gChad)* 0.2, # make it negative  !! Let Debjani know
         CH4_cum_gha = cumsum(CH4_net_gChad)) %>%  # this sums the cumulative amount of daily CH4
  #  mutate(CH4_net_gChad=CH4_emis_gChad-CH4_oxid_gChad) %>%
  arrange(year,dayofyear)

Day_summary <- Day_summary_raw[Day_summary_raw$year <= end_fut_period_year,]



# DayGM_ghaday <- Day_summary[,c("time","dayofyear","CH4_net_gChad")] %>%
#   mutate(year=floor(time),
#          date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)
# 
# 
# # # Daily CH4 Emissions?
# DayGM_cum_gha <- DayGM_ghaday[,c("year","dayofyear","date","CH4_net_gChad")] %>%
  


############# ANNUAL ############# 
# CH4 Emissions annual
DayGM_ann_gha <- Day_summary_raw %>%
  group_by(year) %>%
  summarize(CH4Emissions_ghayr=sum(CH4_net_gChad))
############ ####### #############

###############################################################################
## Forms of N
###############################################################################

# Adding soil_df Bulk Density here AD
ObsBD<-data.frame(mean_BD = mean(as.numeric(soil_df$bdfiod_value_avg)))


Day_base_soiln <- fread(paste0(daycent_path,paste0("soiln_base_",scenario_name2,".out")),
                        # widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                        col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                    "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                    "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                    "NO3_ppm11","NO3_ppm12"),
                        skip=1) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2,
         NO3_10to60cm_ppm=NO3_ppm3+NO3_ppm4+NO3_ppm5,
         #NO3_kgha=(weight of soil in kg/ha, using bulk density from 
         # 2_Create_soil_data-setup2_KBS to soil depth in m)*ppm which = mg/kg
         #          then divide by 1000000 mg/kg conversion factor
         NO3_kgha=((0.10*10000*ObsBD$mean_BD*1000)*NO3_ppm)/1000000, # Bulk density obtained from soils_df AD
         NO3_hgha=NO3_kgha/10, 
         NO3_10to60cm_kgha=((0.50*10000*ObsBD$mean_BD*1000)*NO3_10to60cm_ppm)/1000000, 
         NO3_10to60cm_hgha=NO3_10to60cm_kgha/10, 
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         NO3_2cm_kgha=((0.02*10000*ObsBD$mean_BD*1000)*NO3_ppm0)/1000000,
         NO3_5cm_kgha=((0.03*10000*ObsBD$mean_BD*1000)*NO3_ppm1)/1000000,
         NO3_10cm_kgha=((0.05*10000*ObsBD$mean_BD*1000)*NO3_ppm2)/1000000,
         NO3_20cm_kgha=((0.1*10000*ObsBD$mean_BD*1000)*NO3_ppm3)/1000000,
         NO3_40cm_kgha=((0.2*10000*1.2*1000)*NO3_ppm4)/1000000,
         NO3_60cm_kgha=((0.2*10000*1.6*1000)*NO3_ppm5)/1000000,
         NO3_0to60cm_kgha=NO3_2cm_kgha+NO3_5cm_kgha+NO3_10cm_kgha+
           NO3_20cm_kgha+NO3_40cm_kgha+NO3_60cm_kgha,
         NO3_ppm_top10cm=NO3_ppm0+NO3_ppm1+NO3_ppm2+NO3_ppm3)  # this top cm was a calc done in old script 
#just named NO3_ppm, so I added the top 10 cm to name to 
# be clear on what it is. Seems superfluous though, don't know why needed.


Day_soiln_all <- Day_base_soiln[Day_base_soiln$year <= end_fut_period_year,]

# water-filled pore space

# Day_base_wfps <- fread(file.path(daycent_path2,paste0("wfps_base_",scenario_name2,".out")),
#                           # widths=c(8,5, rep(9, 13)), # 13 soil layers
#                           col.names=c("time","dayofyear", paste0("wfps_layer", 0:nlayers))) %>%
#   mutate(year=floor(time),
#          date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)


# CO2resp Heterotrophic CO2 respiration for the day (g C ha-1 d-1)
DayGN_ghaday <- Day_summary[,c("time","dayofyear","N2O_gNhad", "CO2resp")] %>% # AD adding CO2resp g C ha-1 d-1
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         N2OEmissions_ghaday=N2O_gNhad,
         CO2resp_gha = CO2resp
         )


# ############# ANNUAL ############## AD don't think needed now
# # N20 Emissions and added CO2 resp - annually
DayGN_ann_gha <- DayGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2O_gNhad),
            CO2resp_ghayr=sum(CO2resp))
# ############ ####### #############


# Reported in daily output
DayGN_cum_gha <- DayGN_ghaday[,c("year","dayofyear","date","N2O_gNhad","CO2resp")] %>% 
  mutate(N2O_cum_gha = cumsum(N2O_gNhad),
         N2O_emit_gha = N2O_gNhad,
         CO2cum_gha = cumsum(CO2resp),
         CO2_emit_gha = CO2resp)%>%
  select(-N2O_gNhad, -CO2resp)

# DayGN_cum_calib <- DayGN_ghaday[DayGN_ghaday$date %in% pull(ObsGas[!is.na(ObsGas$N2O_N),], date),] %>%
#   group_by(year) %>%
#   summarize(tot_N2O_ghayr=sum(N2O_gNhad))


DayGC_ghaday <- Day_summary[,c("time","dayofyear","NOflux")] %>% # changed from Day_summary_base, N2Oflux
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)



#**********************************************************************
###############################################################################
# Soil Organic Carbon
###############################################################################

lis_output <- fread(paste0(daycent_path,paste0("sched_base_",scenario_name2,".lis")),
                                                  col.names = c("time","somsc_gm2","somtc","somte(1)",
                                       "crpval","cinput","somse(1)","petann",
                                       "tminrl(1)","minerl(1,1)","minerl(2,1)",
                                       "minerl(3,1)","minerl(4,1)","minerl(5,1)",
                                       "minerl(6,1)","minerl(7,1)","minerl(8,1)",
                                       "aglivc","bglivcj","bglivcm","cgrain",
                                       "crmvst","hi","clitad(1)","clitad(2)",
                                       "elitad(1,1)","elitad(2,1)"),
                         colClasses=c("numeric","numeric","numeric","numeric",
                                      "character","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric"),
                         skip=45) %>%
  mutate(year=floor(time))


DayC_Mgha <- lis_output %>%   #[,c("time","somsc_gm2","year")] Just grab all for potential use later
  mutate(SOC_Mghayr=round(somsc_gm2/100,1)
  )


# Also get SOC from the soils processing
# SOCgNATSGO<-data.frame(SOCgNATSGO = sps$SOC)

# CO2 out
# Day_base_co2 <- fread(paste0(daycent_path,paste0("co2_base_",scenario_name2,".out")),
#                            # widths=c(8,6,rep(14, 13)),
#                            col.names=c("time","dayofyear", paste0("CO2_ppm", 0:nlayers)), 
#                            skip=1)%>%
#   mutate(year=floor(time),
#          date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)


# Annual
# grspann(1) – total annual growth respiration for grass/crop system (g C m-2 yr-1)
gresp_out<- fread(paste0(daycent_path,paste0("gresp_base_",scenario_name2,".out")))%>%
  select(time, dayofyr, 'grspann(1)')%>%
  filter(dayofyr==365)%>% # only want the last day of the year
  mutate(year=floor(time),
         date=as.Date(dayofyr,origin=paste0(as.character(year),"-01-01"))-1)

# Annual
# mrspann(1) (Column 27) – Accumulator for annual maintenance respiration for grass/crop (g C m-2 yr-1)
mresp_out<- fread(paste0(daycent_path,paste0("mresp_base_",scenario_name2,".out")))%>%
  select(time, dayofyr, 'mrspann(1)') %>%
  filter(dayofyr==365)%>% # only want the last day of the year
  mutate(year=floor(time),
         date=as.Date(dayofyr,origin=paste0(as.character(year),"-01-01"))-1)

# Daily
# sysc (Column 6) – System C (g C m-2) (livec + deadc + soilc) This is what we're calling daily SOC (does not seem to include inorganic carbon, but it's hard for me to tell)
sysc_out<- fread(paste0(daycent_path,paste0("sysc_base_",scenario_name2,".out")))%>%
  select(time, dayofyr, sysc, soilc)%>%
  mutate(year=floor(time), date=as.Date(dayofyr,origin=paste0(as.character(year),"-01-01"))-1,
         SOC_Mgha=sysc*0.000001) # convert g C m-2 to Mg C ha-1



# ############# ANNUAL ############## 
# # CO2 Emissions  - annually - delete
# Day_ann_co2 <- Day_base_co2 %>%
#   group_by(year) %>%
#   summarize(N2OEmissions_ghayr=sum(N2O_gNhad))
# ############ ####### #############


#**********************************************************************

######################## ############# ########################
######################## ANNUAL OUTPUT ########################
######################## ############# ########################

# soybeans and rotation is different because soybeans weren't widely grown until 1920, so corn is crop from 1850 until 1920
# so both versions have two columns for crop, so columns needs to be processed differently than above
# set crop column with if statement
if(crop=="Soybean" | crop=="Rotation"){
  crop_names_units <-paste0(c("Maize" ,"Soybean"), "Yld_Mgha")
  crop_names <-c("Maize" ,"Soybean")
}
if(crop=="Wheat" | crop == "Cotton" | crop == "Maize"){
  crop_names_units <-paste0(crop, "Yld_Mgha") 
  crop_names<-crop
}

print(paste0("creating annual output table for ", paste(crop_names)))


# Merge all the annual data together

output_annual_data<-
  left_join(DayY_Mgha_pivwid[,c("year",crop_names)], DayC_Mgha[,c('SOC_Mghayr', 'time')], by = c("year" = "time"))%>%
  left_join(DayGN_ann_gha, by = "year")%>%
  left_join(DayGM_ann_gha, by = "year")%>%
  left_join(gresp_out[,c('year', 'grspann(1)')], by = "year")%>%
  left_join(mresp_out[,c('year', 'mrspann(1)')], by = "year")


# Get "crop" to CropnameMgha (with units)
output_annual_data[,crop_names_units]<-output_annual_data[,crop_names]

colnames(output_annual_data)

# Add columns for reference
output_annual_data$model_name<- 'Daycent'
output_annual_data$scenario_name<- scenario_name2
output_annual_data$climate_scenario_num<- clim_scenario_num
output_annual_data$mgmt_scenario_num<- mgmt_scenario_num

# Doing a dplyr select instead of the below
output_annual_data2<-output_annual_data%>%
  mutate(grespann1_ghayr = `grspann(1)`,
         mrespann1_ghayr = `mrspann(1)`)%>%
  select(annual_output_columns, {crop_names_units} ,grespann1_ghayr, mrespann1_ghayr) 

# ,"SoyYld_Mgha","WheatYld_Mgha" removed these from below AD
# colnames(output_annual_data) <- c("year", 
#                                   paste0(crop_names, "Yld_Mgha"),
#                                   "SOC_Mghayr",
#                                   'N2OEmissions_ghayr', 
#                                   'CO2resp_ghayr',
#                                   'CH4Emissions_ghayr',
#                                   'grespann1_ghayr',
#                                   'mrespann1_ghayr',
#                                   "model_name",
#                                   "scenario_name",
#                                   "climate_scenario_num",
#                                   "mgmt_scenario_num") #,"mgmt_scenario_opt_num")

# Delete
# # combining this way as an easy way to ensure SOC is in first set of columns. Otherwise it's lost in the back.
# # probably most important columns are the extra carbon pools
# output_annual_data<-left_join(output_annual_data, select(DayC_Mgha, -SOC_Mgha, -time), by = 'year')

# DayGN_ann_gha

# write out the annual results
fwrite(output_annual_data,file=daycent_annual_out,col.names=T,row.names=F,sep=",",append=F)

if(file.exists(daycent_annual_out)){
  cat("************************************\n")
  print(paste0("Annual results compilation file written for ", scenario_name2))
  cat("************************************\n")
}

######################## ############# ########################
######################## ############# ########################
######################## ############ ########################
######################## DAILY OUTPUT ########################
######################## ############ ########################


################################################################################
# this table should grab everything, consider changing to data.table::merge
output_daily<-
  # left_join(Day_summary, select(Day_base_co2, -dayofyear, -time), by =  c('year','date'))%>%
  left_join(Day_summary, select(Day_soiln_all, -dayofyear, -time), by =  c('year','date'))%>%
  left_join(select(DayM_V_all, -dayofyear, -time), by =  c('year','date'))%>%
  left_join(select(Day_base_soiltavg, -dayofyear, -time), by =  c('year','date'))%>%
  left_join(select(DayGN_cum_gha, -dayofyear), by =  c('year','date')) %>%
  left_join(select(sysc_out, -dayofyr, -time), by =  c('year','date')) %>%
  left_join(select(DayGN_ghaday, -dayofyear, -time), by =  c('year','date')) %>%
  left_join(Day_NPP, by =  c('year','date'))%>%
  mutate(scenario_name = scenario_name2,
         mgmt_scenario_num = mgmt_scenario_num,
         climate_scenario_num = clim_scenario_num,
         model_name = 'Daycent'
  )

output_daily2<-output_daily%>%
  select(daily_outputs_columns, everything())

fwrite(output_daily2,file=daycent_daily_out,col.names=T,row.names=F,sep=",",append=F)

if(file.exists(daycent_daily_out)){
  cat("************************************\n")
  print(paste0("Daily results compilation file written for ", scenario_name2))
  cat("************************************\n")
}

# run_time <- round(toc(echo=T)/60,1)
# print(paste0("Results run time is ",run_time," minutes"))

######################## ############ ########################

