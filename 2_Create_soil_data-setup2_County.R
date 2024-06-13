#######################################
# Function: "2_Create_soil_data-setup2_KBS"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Description: Downloads the closest SSURGO soil profile for the site and updates
# the bulk density and C content with site values. Fills in treatment differences
# and creates enough soil attributes for APSIM, Daycent and LDNDC.
#######################################
# AUDIT TRAIL
# 2/23/2023: Calculated SAT, LL15, DUL, and KS with Saxton and Rawls (2006).
#######################################

#!!!!!!!!!! Note: 
######## Limit to 100 cm depth to avoid data issues, and this is also the depth
######## limit on the study ########### But unsure how to do this for APSIM's
######## custom format.
#!!!!!!


# library(apsimx)
library(stringr)
library(dplyr)
library(tidyverse)
library(soiltexture)
library(xml2)
library(lubridate)
library(soilDB)
library(sf)
library(zoo)


# local constants
# 
# 
# Cpct_0to20 <- as.numeric(ObsBD$mean_BD)  #ObsBD file in )_Observations_and_constants script
# Cpct_20to40 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
#                                   ObsBDdeep_mean$section=="Middle","mean_BD"])
# Cpct_40to60 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
#                                   ObsBDdeep_mean$section=="Middle","mean_BD"])
# Cpct_60to80 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
#                                   ObsBDdeep_mean$section=="Deep","mean_BD"])
# Cpct_80to200 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
#                                   ObsBDdeep_mean$section=="Deep","mean_BD"])


###########################
#import and clean
###########################


# download soil data from SSURGO for the lat/lon into a list of "soil.profile"
# classes, pre-formatted for APSIM

# Example to get to
#1  0 2 1.11 0.26 0.06753815 0 0.01 0.43 0.19 0.0174 0.01 1e-04 5.5
#2  2 5 1.11 0.26 0.06753815 0 0.04 0.43 0.19 0.0174 0.01 1e-04 5.5
#3  5 10 1.11 0.26 0.06753815 0 0.25 0.43 0.19 0.0174 0.01 1e-04 5.5
#4  10 20 1.11 0.26 0.06753815 0 0.3 0.43 0.19 0.0174 0.01 1e-04 5.5
#5  20 40 1.2 0.26 0.06753815 0 0.15 0.43 0.19 0.0086 0.01 1e-04 5.5
#6  40 60 1.6 0.26 0.06753815 0 0.1 0.43 0.19 0.0086 0.01 1e-04 5.5
#7  60 80 1.55 0.246 0.13822352 0 0.05 0.51 0.23 0.00466 0.01 5.5e-05 6.5
#8  80 100 1.55 0.142 0.06916603 0 0.04 0.71 0.12 0.0038 0.01 0.00055 6.5
#9  100 120 1.55 0.069 0.02266792 0 0.03 0.87 0.05 0 0.01 0.0055 6.5
#10  120 140 1.55 0.069 0.02266792 0 0.02 0.87 0.05 0 0.01 0.0055 6.5
#11 140 160 1.55 0.069 0.02266792 0 0.01 0.87 0.05 0 0.01 0.0055 6.5
#12  160 180 1.55 0.033 0.02266792 0 0 0.95 0.01 0 0.01 0.0055 6.5
#13 180 200 1.55 0.033 0.02266792 0 0 0.95 0.01 0 0.01 0.0055 6.5

# What we have with gNATSGO data
# 1:     0 to 2 1.364556 23.41935 6.622648e+00 17.50934 58.66808 2.3067876
# 2:     2 to 5 1.364711 23.41935 6.622715e+00 17.50934 58.66808 2.2890927
# 3:    5 to 10 1.365027 23.41935 2.049073e+06 17.50934 58.66808 2.2536962
# 4:   10 to 20 1.359556 24.21606 6.868683e+00 16.86304 58.51747 1.9278830
# 5:   20 to 30 1.359348 26.55759 7.038710e+00 16.08226 56.82251 1.4138239 - change to 20 to 40
# 6:   30 to 45 1.377460 27.84241 7.176680e+00 16.32372 55.29657 0.9819960 - combine with above row
# 7:   45 to 60 1.378038 26.88448 7.358669e+00 16.95477 55.35329 0.7787567  - change to 40 to 60
# 8:   60 to 75 1.376868 24.84610 7.582393e+00 18.63797 55.70948 0.6488038  - change to 60 to 80
# 9:   75 to 90 1.369321 22.86122 7.715928e+00 19.54597 56.78636 0.5656519 - change to 80 to 100
# 10:  90 to 105 1.350618 20.44301 7.854570e+00 20.39126 58.35444 0.4597984  - change to 100 to 120
# 11: 105 to 120 1.304173 18.91606 7.610484e+00 20.49187 56.82735 0.4162702 - change to 120 to 140
# 12: 120 to 150 1.304180 18.86472 7.614046e+00 20.55222 56.82043 0.4148858 - change to 140 to 160
# 13: 150 to 180 1.303676 18.83609 7.614919e+00 20.59261 56.80793 0.4176143 - change to 160 - 180
# 14: 180 to 200 1.252628 18.00672 7.361761e+00 20.01358 54.65444 0.3991936



soils<-fread(list.files(soil_data_path, full.names = TRUE, pattern = paste0("_", county_geoid, "_")))%>%
  select(-GEOID)

# tell Debjani!!!! Murray County georgia had an erroneous pH value as well as really strange values overall
#, so using this to avoid
# note: zoo library loaded in climate script run prior
soils<-mutate(soils, pH = ifelse(pH > 7.5, NA, pH)) # remove pH values greater than 7.5, some are obviously in error

# Stopped doing this because of error in Ks values in saxton_rawls_df
# soils<-mutate(soils, pH = ifelse(BD < 1, NA, pH)) # remove pH less than 1, less than this throws an error
# soils<-mutate(soils, BD = ifelse(BD < .5, NA, pH)) # remove bulk density values less than .5, less than this throws an error

soils<-na.locf(soils) # This sets NA values to the last non-NA value


# soils<-mutate(soils, pH = ifelse(pH<5.5, 5.5, ifelse(pH>7.5, 7.5, pH))) # keep pH within 5.5 and 7.5

# to get gNATSGO correct, at present, we need to re-arrange the rows so it matches the way soils data was
# arranged before. To do this, we'll manipulate the data as follows:
soils_1<-soils[1:4,] # get rows 1 to 4
# collapse rows 5, 6 so that it's a convenient 20 to 40, it just takes several steps as I am currently working through it
# might be an easier way?
soils_2<-soils[5:6,] # get rows 5, 6
# get column means to combine
soils_2_<-as.data.frame(as.list(colMeans(soils_2[,2:7]))) # named numbers as list, as dataframe
soils_2_1<-data.frame('Depth_cm'=c('20 to 40')) # add depth column
soils_2_2<-cbind(soils_2_1, soils_2_) # combine

soils_3<-soils[7:14,] # for rows 7 to 13, rename as noted above
soils_3$Depth_cm<-c('40 to 60', '60 to 80', '80 to 100', '100 to 120', '120 to 140', '140 to 160', '160 to 180', '180 to 200')

# combine everything back
soils_new<-rbind(soils_1, soils_2_2, soils_3)



# print(paste0('getting soil data for lat ', latitude, ' and lon ', longitude, " at ", site_name))
# 
# sps_raw <- get_ssurgo_soil_profile(lonlat = c(longitude, latitude), nsoil=1)
# sps <- sps_raw

# edit attributes from site data and APSIM calibration, relative to each scenario
# based on deep soil cores from 2001 and APSIM calibration

# sps[[1]]$crops <- c('maize','soybean','wheat')
## Bulk density presents an unusual challenge in that it needs to be fixed at the 
## control plot BD for equivalent soil mass between the initial C at land conversion 
## and the current day, because APSIM will compute the SOC stock as BD*Carbon*depth.
## HOWEVER, the other flow attributes (AirDry, LL15, DUL, SAT) need to reflect the
## actual BD at the site (?? I believe) so that the system functions as it actually
## is. So APSIM should be initially calibrated with the treatment BD, then the
## BD changed to the control plot.
##
## soil layers are in 20 cm increments to 200 cm
# sps[[1]]$soil$BD <- c(Cpct_0to20, 1.2, Cpct_40to60, Cpct_60to80, Cpct_80to200, 
#                           Cpct_80to200, Cpct_80to200, Cpct_80to200, Cpct_80to200, Cpct_80to200) # can we remove because it's APSIM?
## APSIM Classic has a lower limit of 0.01 C content, so bottom 5 layers with 0
## were replaced with 0.01
# sps[[1]]$soil$Carbon <- if(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4:7))
#                            c(0.87, 0.43, 0.43, 0.233, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else 
#                         if(mgmt_scenario_num==2)
#                            c(0.99, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else
#                         if(mgmt_scenario_num==3)
#                            c(0.93, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1)

# sps[[1]]$soil$Carbon <- c(0.87, 0.43, 0.43, 0.233, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1)
# sps[[1]]$soil$ParticleSizeClay <- c(19, 19, 19, 23, 12, 5, 5, 5, 1, 1)
# sps[[1]]$soil$ParticleSizeSilt <- c(38, 38, 38, 26, 17, 8, 8, 8, 4, 4)
# sps[[1]]$soil$ParticleSizeSand <- c(43, 43, 43, 51, 71, 87, 87, 87, 95, 95)

# sps$Carbon <- c(0.87, 0.43, 0.43, 0.233, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1)
# sps$ParticleSizeClay <- c(19, 19, 19, 23, 12, 5, 5, 5, 1, 1)
# sps$ParticleSizeSilt <- c(38, 38, 38, 26, 17, 8, 8, 8, 4, 4)
# sps$ParticleSizeSand <- c(43, 43, 43, 51, 71, 87, 87, 87, 95, 95)


sps_0<-soils_new%>%
  mutate(ParticleSizeClay = Clay, ParticleSizeSilt = Silt, ParticleSizeSand = Sand, Carbon = SOC, PH = pH)

# This was fixed using Zoo library na.conf, so not necessay
sps_0$pH<-ifelse(sps_0$pH>9, NA, sps_0$pH) # I found a really high pH in the data, which is not possible, so am imputing as a quick fix here
# TODO need to go into gNATSGO processing and see what it might be

sps_0<-sps_0%>%fill(pH, .direction = "down") # fill from tidyr

sps<-sps_0%>%
  mutate(ParticleSizeClay = Clay, ParticleSizeSilt = Silt, ParticleSizeSand = Sand, Carbon = SOC, PH = pH)

##########################################################################
## save this much to a data frame and calculate the water attributes
# extract just soil data into a dataframe
# soil_water_raw <- sps[[1]]$soil
soil_water_raw <- sps

saxton_rawls_df <- soil_water_raw %>%
  mutate(sand_frac = ParticleSizeSand/100,
         silt_frac = ParticleSizeSilt/100,
         clay_frac = ParticleSizeClay/100,
         OM_frac = Carbon*1.724/100, # organic matter fraction
         O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
           0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
           0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
         LL15 = O1500t + (0.14 * O1500t - 0.02), # permanent wilting point, %
         # LL15 = ifelse(O1500t + (0.14 * O1500t - 0.02)<=0, .000001, 
         #               O1500t + (0.14 * O1500t - 0.02)), # permanent wilting point, %   #!!!! AD WARNING: this is a hack to avoid negative values
         O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
           0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
           0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
         DUL = O33t + (1.283*O33t^2 - 0.374*O33t - 0.015), # field capacity, %
         SAT = 1 - BD/2.65, # moisture at saturation, %; 2.65 = assumed particle density
         B = (log(1500) - log(33))/(log(DUL) - log(LL15)), # moisture-tension coefficient
         lamda = 1/B, # slope of tension-moisture curve
         Ks = 1930*(SAT-DUL)^(3-lamda), # saturated conductivity (mm h-1) # talk to Debjani about taking cube of non-integer and R problems with that
         Ks_mmday = Ks*24,
         Ks_cmsec = Ks/10/60/60,
         Ks_cmhr = Ks/10,
         phaq_value_avg = PH, # use soil ph -AD
           bdfiod_value_avg = BD,# use soil bulk density -AD
         AirDry = 0.1318797) # using average from geoid 13023 TODO CHECK WITH DEBJANI
# sps[[1]]$soil$SAT <- saxton_rawls_df$SAT
# sps[[1]]$soil$AirDry <- saxton_rawls_df$AirDry
# sps[[1]]$soil$LL15 <- saxton_rawls_df$LL15
# sps[[1]]$soil$DUL <- saxton_rawls_df$DUL
# sps[[1]]$soil$bdfiod_value_avg <- saxton_rawls_df$bdfiod_value_avg
# sps[[1]]$soil$phaq_value_avg <- saxton_rawls_df$phaq_value_avg
sps$SAT <- saxton_rawls_df$SAT
sps$AirDry <- saxton_rawls_df$AirDry
sps$LL15 <- saxton_rawls_df$LL15
sps$DUL <- saxton_rawls_df$DUL
sps$bdfiod_value_avg <- saxton_rawls_df$bdfiod_value_avg
sps$phaq_value_avg <- saxton_rawls_df$phaq_value_avg
sps$KS<-saxton_rawls_df$Ks
# sps$Ks_mmday<-saxton_rawls_df$Ks_mmday
# sps$Ks_cmsec<-saxton_rawls_df$Ks_cmsec
# sps$Ks_cmhr<-saxton_rawls_df$Ks_cmhr

####################################################################
## continue with remaining soil elements

# sps[[1]]$soil$SoilCNRatio <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
# sps[[1]]$soil$PH <- c(5.5, 5.5, 5.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5)
# sps[[1]]$soil$Maize.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
#                                 0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
# sps[[1]]$soil$Soybean.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
#                               0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
# sps[[1]]$soil$Wheat.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
#                                 0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
# if(mgmt_scenario_num==3) {
# sps[[1]]$soil$WhiteClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
# sps[[1]]$soil$WhiteClover.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
#                                 0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
# sps[[1]]$soil$WhiteClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
# sps[[1]]$soil$RedClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
# sps[[1]]$soil$RedClover.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
#                                 0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
# sps[[1]]$soil$RedClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
# sps[[1]]$soil$Oats.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
# sps[[1]]$soil$Oats.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
#                                 0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
# sps[[1]]$soil$Oats.XF <- c(1,1,1,1,1,1,1,1,1,1)
# }

# extract just soil data into a dataframe
# soil_df_raw <- sps[[1]]$soil
soil_df_raw <- sps

# add three more depths at the top (for Daycent, recommended for trace gas subroutines),
# then add new columns which Daycent also needs

# three_layers <- rbind(soil_df_raw[1,], soil_df_raw[1,], soil_df_raw[1,])
# three_layers[1,"Depth"] <- "0-2"
# three_layers[1,"Thickness"] <- 20
# three_layers[1,"FOM"] <- 25
# three_layers[2,"Depth"] <- "2-5"
# three_layers[2,"Thickness"] <- 30
# three_layers[2,"FOM"] <- 25
# three_layers[3,"Depth"] <- "5-10"
# three_layers[3,"Thickness"] <- 50
# three_layers[3,"FOM"] <- 50
# 
# names(three_layers)
names(soil_df_raw)

soil_df_raw<-soil_df_raw%>%
  mutate(Depth = gsub(' to ', '-', Depth_cm))%>%
  select(-Depth_cm)


soil_df <- #three_layers %>%
  # rbind(soil_df_raw) %>%
  soil_df_raw%>%
  mutate(upper_depth_cm = as.numeric(word(Depth, 1, sep="-")),
         lower_depth_cm = as.numeric(word(Depth, 2, sep="-")),
         root_fraction = c(0.01, 0.04, 0.25, 0.30, 0.15, 0.1, 0.05, 0.04, 0.03,
                           0.02, 0.01, 0, 0), 
         sand_frac = ParticleSizeSand/100,
         clay_frac = ParticleSizeClay/100,
         OM_frac = Carbon*2/100,
         deltamin = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 
                      0.01, 0.01, 0.01, 0.01),
         ksat_cmsec = KS/(10*24*60*60),
         evap_coef = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
# soil_df[4,"Depth"] <- "10-20"  # What is this for?
# soil_df[4,"Thickness"] <- 100
# soil_df[4,"FOM"] <- 50
# soil_df[4,"upper_depth_cm"] <- 10

soil_df$KS_cmmin <- soil_df$KS * (1/(10*24*60))

soil_df$LL15_dm3m3 <- soil_df$LL15*1000
soil_df$DUL_dm3m3 <- soil_df$DUL*1000
soil_df$LL15_mmm3 <- soil_df$LL15*1000000000*0.001
soil_df$DUL_mmm3 <- soil_df$DUL*1000000000*0.001

# add orgC as fraction for LDNDC
soil_df$orgC_fraction <- soil_df$Carbon/100

# calculate soil type code from soil texture

soil_texture_df <- soil_df[1,c("sand_frac","clay_frac")]
colnames(soil_texture_df) <- c("SAND","CLAY")
soil_texture_df$SILT <- 1 - (soil_texture_df$SAND + soil_texture_df$CLAY)
soil_texture_df <- soil_texture_df %>%
  mutate(SAND=SAND*100,
         SILT=SILT*100,
         CLAY=CLAY*100)

soil_type_ar <- TT.points.in.classes(tri.data=soil_texture_df,class.sys="USDA.TT") # soiltexture library

## find the non-zero column which is the soil type - for LDNDC
find_col <- names(which(colSums(soil_type_ar)==1))
soil_type_code <- toupper(if_else(find_col=="Cl","clay",
                          if_else(find_col=="Lo","loam",
                          if_else(find_col=="Si","silt",
                          if_else(find_col=="Sa","sand",
                          if_else(find_col=="SiClLo","slcl",
                          if_else(find_col=="SaClLo","sncl",
                                  find_col))))))
                          )

# LDNDC soil table
soil_df_L<-soil_df%>%
  select(upper_depth_cm, lower_depth_cm, bdfiod_value_avg, DUL_dm3m3, LL15_dm3m3, evap_coef, #DUL_dm3m3, LL15_dm3m3
         root_fraction, sand_frac, clay_frac, OM_frac, deltamin, ksat_cmsec, phaq_value_avg, 
         orgC_fraction, KS_cmmin)%>%
  mutate(soil_type_code = soil_type_code)

soil_df_L$Thickness<-c(2, 3, 5, 10, 20, 20, 20, 20, 20, 20, 20, 20, 20)





# change the significant digits of columns to match original soil.in file
# sig digs were: 0 0 2 2 8 0 2 2 2 4 2 scinum 1
# select columns needed
soil_df<-soil_df[,c("upper_depth_cm","lower_depth_cm","bdfiod_value_avg","DUL","LL15","evap_coef",
                  "root_fraction","sand_frac","clay_frac","OM_frac",
                  "deltamin","ksat_cmsec","phaq_value_avg")]

# change significant digits
# upper_depth_cm
# lower_depth_cm
soil_df$bdfiod_value_avg<-formatC(soil_df$bdfiod_value_avg, digits=2, format="fg", flag="#")
soil_df$DUL<-formatC(soil_df$DUL, digits=2, format="fg", flag="#")
soil_df$LL15<-formatC(soil_df$LL15, digits=8, format="fg", flag="#")
soil_df$evap_coef<-formatC(soil_df$evap_coef, digits=0, format="fg", flag="#")

soil_df$root_fraction<-formatC(soil_df$root_fraction, digits=2, format="fg", flag="#")
soil_df$sand_frac<-formatC(soil_df$sand_frac, digits=2, format="fg", flag="#")
soil_df$clay_frac<-formatC(soil_df$clay_frac, digits=2, format="fg", flag="#")
soil_df$OM_frac<-formatC(soil_df$OM_frac, digits=4, format="fg", flag="#")

soil_df$deltamin<-formatC(soil_df$deltamin, digits=2, format="fg", flag="#")
soil_df$ksat_cmsec<-formatC(soil_df$ksat_cmsec, format = "e", digits = 2)  # scinum
soil_df$phaq_value_avg<-formatC(soil_df$phaq_value_avg, digits=2, format="fg", flag="#")


####* NOTE: Will need to address issue of negative values in lower limit,
####* as well as LL15-deltamin resulting in a negative value
####* 
####* 
####*

# notes
#?check_apsimx_soil_profile
#> ?compare_apsim_soil_profile - can return diffs between profiles
#> # cmp created from example - shows bias, etc. between two sites
#> 

######################################
#### testing
######################################
# hydrological, from Saxton and Rawls (2006)
# full_soil_prof_df <- soil_df_raw %>%
# mutate(sand_frac = ParticleSizeSand/100,
#        silt_frac = ParticleSizeSilt/100,
#        clay_frac = ParticleSizeClay/100,
#        OM_frac = Carbon*1.724/100, # organic matter fraction
#        O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
#          0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
#          0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
#        LL15 = O1500t + (0.14 * O1500t - 0.02), # permanent wilting point, %
#        O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
#          0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
#          0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
#        DUL = O33t + (1.283*O33t^2 - 0.374*O33t - 0.015), # field capacity, %
#        SAT = 1 - BD/2.65, # moisture at saturation, %; 2.65 = assumed particle density
#        B = (log(1500) - log(33))/(log(DUL) - log(LL15)), # moisture-tension coefficient
#        lamda = 1/B, # slope of tension-moisture curve
#        Ks = 1930*(SAT-DUL)^(3-lamda), # saturated conductivity (mm h-1)
#        Ks_mmday = Ks*24,
#        Ks_cmsec = Ks/10/60/60,
#        Ks_cmhr = Ks/10)
       
       

