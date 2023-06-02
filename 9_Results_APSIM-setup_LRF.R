#######################################
# File: "9_Results_APSIM-setup_LRF.R"
# Author: "Ellen Maas"
# Date: "7/22/2022"
# Description: Imports results from APSIM output. Changed to all drive
# from APSIM Classic.
#
#######################################
# Audit Log
# 7/22/2022: Created script
# 2/23/2023: Modified to use all APSIM Classic.
#
#######################################
print(paste0("Starting 9_Results_APSIM-setup_",site_name,".R"))

#library(apsimx)
 library(magrittr)
 library(lubridate)
library(dplyr)
library(tidyr)



#**********************************************************************

# import APSIM modeled points

if(mgmt_scenario_grp!=3 & mgmt_scenario_grp!=8) {
APSIM_out_raw <- read.fwf(paste0(apsim_path,"scen_",scenario_name,".out"),skip=4,
                      widths = c(15,15,22,21,15,15,24,23,15,15,15,
                                 20,20,20,20,20,19,19,19,19,19,15),
                      col.names = c("Date","BulkDensity_gcc(1)","SorghumYield_kgha",
                                    "CottonYield_kgha","VolH2O_5cm","SoilTemp_5cm_C",
                                    "sorghum_biomass_kgha","cotton_biomass_kgha",
                                    "dul_10cm","sat_10cm","ph_10cm",
                                    "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                    "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                    "N2O_bylayer_kgha(5)","oc_bylayer_pct(1)",
                                    "oc_bylayer_pct(2)","oc_bylayer_pct(3)",
                                    "oc_bylayer_pct(4)","oc_bylayer_pct(5)",
                                    "BulkDensity_gcc(2)"),
                      colClasses = c("character","numeric","numeric",
                                     "numeric","numeric","numeric",
                                     "numeric","numeric",
                                     "numeric","numeric","numeric",
                                     "numeric","numeric",
                                     "numeric","numeric",
                                     "numeric","numeric",
                                     "numeric","numeric",
                                     "numeric","numeric",
                                     "numeric"),
                      check.names = FALSE, header=FALSE) %>%
  mutate(date=as.Date(Date, "%d/%m/%Y"),
         year=year(date),
         month=month(date),
         day=day(date),
         SorghumYield_gm2=SorghumYield_kgha/10,
         CottonYield_gm2=CottonYield_kgha/10,
         TotalSOC_10cm_Mgha=(`BulkDensity_gcc(1)`*10*`oc_bylayer_pct(1)`),# +
          # (`BulkDensity_gcc(1)`*5*`oc_bylayer_pct(2)`),
         N2O_10cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.5))
} else { # includes cover crop columns
  APSIM_out_raw <- read.fwf(paste0(apsim_path,"scen_",scenario_name,".out"),skip=4,
                        widths = c(15,15,22,21,15,15,24,24,23,15,15,15,
                                   20,20,20,20,20,18,18,18,18,18,15),
                        col.names = c("Date","BulkDensity_gcc(1)","SorghumYield_kgha",
                                      "CottonYield_kgha",
                                      "VolH2O_5cm","SoilTemp_5cm_C",
                                      "sorghum_biomass_kgha","rye_biomass_kgha",
                                      "cotton_biomass_kgha",
                                      "dul_5cm","sat_5cm","ph_5cm",
                                      "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                      "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                      "N2O_bylayer_kgha(5)","oc_bylayer_pct(1)",
                                      "oc_bylayer_pct(2)","oc_bylayer_pct(3)",
                                      "oc_bylayer_pct(4)","oc_bylayer_pct(5)",
                                      "BulkDensity_gcc(2)"),
                        colClasses = c("character","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric"),
                        check.names = FALSE, header=FALSE) %>%
    mutate(date=as.Date(Date, "%d/%m/%Y"),
           year=year(date),
           month=month(date),
           day=day(date),
           SorghumYield_gm2=SorghumYield_kgha/10,
           CottonYield_gm2=CottonYield_kgha/10,
           TotalSOC_10cm_Mgha=(`BulkDensity_gcc(1)`*10*`oc_bylayer_pct(1)`),# +
             #(`BulkDensity_gcc(1)`*5*`oc_bylayer_pct(2)`),
           N2O_10cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.5))
}

# limit to future scenario time period
APSIM_out <- APSIM_out_raw[APSIM_out_raw$year <= end_fut_period_year,]

# soil carbon
APSIMC_Mgha <- APSIM_out[APSIM_out$month==7 & APSIM_out$day==15,
                         c("year","TotalSOC_10cm_Mgha")] %>%
  mutate(TotalSOC_10cm_Mgha=round(TotalSOC_10cm_Mgha,1))

# grain yield
APSIMY_Mgha <- APSIM_out[,c("year","SorghumYield_kgha","CottonYield_kgha")] %>%
  group_by(year) %>%
  summarize(SorghumYield_Mgha=round(max(SorghumYield_kgha/1000),3),
            CottonYield_Mgha=round(max(CottonYield_kgha/1000),3))

# biomass yield
## get array of harvest dates
obs_dates<-ObsBiomass$date
date_seq<-seq.Date(from = as.Date('2011-08-15'), to = as.Date('2100-08-15'), by = 'years') # base
date_list<-as.Date(c(obs_dates,date_seq),format="%Y/%m/%d")
APSIMBY_Mgha <- APSIM_out[APSIM_out$date %in% date_list,
                          c("year","sorghum_biomass_kgha","cotton_biomass_kgha")] %>%
  group_by(year) %>%
  summarize(SorghumYield_Mgha=sorghum_biomass_kgha/1000,
            CottonYield_Mgha=cotton_biomass_kgha/1000)

## soil temperature
APSIMT_C <- APSIM_out[,c("date","year","SoilTemp_5cm_C")] %>%
  mutate(SoilTemp_5cm_C=round(SoilTemp_5cm_C,1)) 

# ## soil temperature with bias correction
# APSIMT_C_calib <- APSIM_out[,c("date","year","SoilTemp_10cm_C")] %>%
#   mutate(SoilTemp_5cm_C=round(SoilTemp_5cm_C,1)-soil_temp_bias) 

## volumetric soil moisture
APSIMM_V <- APSIM_out[,c("date","year","VolH2O_5cm")] %>%
  mutate(VolH2O_5cm=round(VolH2O_5cm*100,0))

# ## volumetric soil moisture with bias correction
# APSIMM_V_calib <- APSIM_out[,c("date","year","VolH2O_10cm")] %>%
#   mutate(VolH2O_10cm=round(VolH2O_10cm*100-soil_moist_bias,0))

## bulk density
APSIMB_gcc <- APSIM_out[,c("date","year","BulkDensity_gcc(1)")] %>%
  mutate(BulkDensity_gcc=round(APSIM_out$`BulkDensity_gcc(1)`,2))

## N2O emissions
APSIMGN_ghaday <- APSIM_out[,c("date","year","N2O_10cm_kgha")] %>%
  mutate(N2OEmissions_ghaday = round(N2O_10cm_kgha*1000,2),
         dayofyear = yday(date))

APSIMGN_ann_gha <- APSIMGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2OEmissions_ghaday))

APSIMGN_cum_gha <- APSIMGN_ghaday %>%
  mutate(N2O_gha = cumsum(round(N2O_10cm_kgha*1000,2))) %>%
  select(date,year,N2O_gha)

#**********************************************************************

# write out results for use later in ensemble results
output_annual_data <- cbind(APSIMY_Mgha,APSIMC_Mgha[,"TotalSOC_10cm_Mgha"],
                            "APSIM",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","SorghumYld_Mgha","CottonYld_Mgha",
                                  "SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")
                            
output_daily_data <- cbind(APSIMGN_ghaday[,c("date","year","dayofyear",
                                             "N2OEmissions_ghaday")],
                           APSIMGN_cum_gha[,"N2O_gha"],NA,NA,
                           "APSIM",scenario_name,clim_scenario_num,
                           mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_daily_data) <- c("date","year","dayofyear","N2O_emit_gha","N2O_cum_gha",
                                 "CH4_net_gha","CH4_cum_gha",
                                 "model_name","scenario_name","climate_scenario_num",
                                 "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_APSIM.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_daily_data,file=paste0(results_path,"Daily_results_compilation_",
                                          scenario_name,"_APSIM.csv"),
            col.names=T,row.names=F,sep=",",append=F)


#**********************************************************************
# merge observed and modeled data for graphing model-specific results --------

SorghumYld_Mgha <- merge(ObsYield[ObsYield$crop=="Sorghum",c("year","mean_yield","sd_yield")],
                         APSIMY_Mgha[APSIMY_Mgha$SorghumYield_Mgha != 0,
                                   c("year","SorghumYield_Mgha")],
                         by="year",
                         all=TRUE)%>%
  merge(HistY_Mgha[,c("year","sorghum_yield_mgha")],
        by="year",
        all=TRUE)
colnames(SorghumYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Historical")

SorghumYld_Mgha_piv <- pivot_longer(SorghumYld_Mgha, c(-year,-Obs_sd),
                                    names_to = "source",
                                    values_to = "yield_val")

# remove sd from modeled records; only for observed
SorghumYld_Mgha_piv <- SorghumYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))


##
CottonYld_Mgha <- merge(ObsYield[ObsYield$crop=="Cotton",c("year","mean_yield","sd_yield")],
                        APSIMY_Mgha[APSIMY_Mgha$CottonYield_Mgha != 0,
                                  c("year","CottonYield_Mgha")],
                        by="year",
                        all=TRUE) %>%
  merge(HistY_Mgha[,c("year","cotton_yield_mgha")],
        by="year",
        all=TRUE)
colnames(CottonYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Historical")

CottonYld_Mgha_piv <- pivot_longer(CottonYld_Mgha, c(-year,-Obs_sd),
                                   names_to = "source",
                                   values_to = "yield_val")

# remove sd from modeled records; only for observed
CottonYld_Mgha_piv <- CottonYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
SorghumBioYld_Mgha <- merge(ObsBiomass[ObsBiomass$crop=="Sorghum",c("year","mean_yield","sd_yield")],
                         APSIMBY_Mgha[APSIMBY_Mgha$SorghumYield_Mgha != 0,
                                     c("year","SorghumYield_Mgha")],
                         by="year",
                         all=TRUE)
colnames(SorghumBioYld_Mgha) <- c("year","Observed","Obs_sd","APSIM")

SorghumBioYld_Mgha_piv <- pivot_longer(SorghumBioYld_Mgha, c(-year,-Obs_sd),
                                    names_to = "source",
                                    values_to = "yield_val")

# remove sd from modeled records; only for observed
SorghumBioYld_Mgha_piv <- SorghumBioYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
CottonBioYld_Mgha <- merge(ObsBiomass[ObsBiomass$crop=="Cotton",c("year","mean_yield","sd_yield")],
                        APSIMBY_Mgha[APSIMBY_Mgha$CottonYield_Mgha != 0,
                                    c("year","CottonYield_Mgha")],
                        by="year",
                        all=TRUE)
colnames(CottonBioYld_Mgha) <- c("year","Observed","Obs_sd","APSIM")

CottonBioYld_Mgha_piv <- pivot_longer(CottonBioYld_Mgha, c(-year,-Obs_sd),
                                   names_to = "source",
                                   values_to = "yield_val")

# remove sd from modeled records; only for observed
CottonBioYld_Mgha_piv <- CottonBioYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
             APSIMC_Mgha,
             by="year",
             all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Obs_sd","APSIM")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year,-Obs_sd),
               names_to = "source",
               values_to = "C_val")

# remove sd from modeled records; only for observed
Cstock_Mgha_piv <- Cstock_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
SoilTemp_C <- full_join(APSIMT_C[,c("date","SoilTemp_5cm_C")],
             ObsTemp[,c("date","soil_temperature")],
             by="date")
colnames(SoilTemp_C) <- c("date","APSIM","Observed")

SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
               names_to = "source",
               values_to = "temp_val") %>%
  mutate(year=year(date))

## calibrated
# SoilTemp_C_calib <- merge(ObsTemp[,c("date","soil_temperature")],
#              APSIMT_C_calib[,c("date","SoilTemp_10cm_C")],
#              by="date",
#              all=TRUE)
# colnames(SoilTemp_C_calib) <- c("date","Observed","APSIM")
# 
# SoilTemp_C_piv_calib <- pivot_longer(SoilTemp_C_calib, c(-date),
#                names_to = "source",
#                values_to = "temp_val") %>%
#   mutate(year=year(date))

##
SoilMoist_VSM <- merge(ObsVSM[,c("date","mean_VSM")],
                       APSIMM_V[,c("date","VolH2O_5cm")],
                       by="date",
                       all=TRUE)
colnames(SoilMoist_VSM) <- c("date","Observed","APSIM")

SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date),
               names_to = "source",
               values_to = "h2o_val") %>%
  mutate(year=year(date))

## calibrated
# SoilMoist_VSM_calib <- merge(ObsVSM[,c("date","mean_VSM")],
#                        APSIMM_V_calib[,c("date","VolH2O_10cm")],
#                        by="date",
#                        all=TRUE)
# colnames(SoilMoist_VSM_calib) <- c("date","Observed","APSIM")
# 
# SoilMoist_VSM_piv_calib <- pivot_longer(SoilMoist_VSM_calib, c(-date),
#                names_to = "source",
#                values_to = "h2o_val") %>%
#   mutate(year=year(date))

##
# SoilBD_gcc <- merge(ObsBD[,c("year","mean_BD")],
#                     APSIMB_gcc[APSIMB_gcc$date=="1996-01-01",c("year","BulkDensity_gcc")],
#                     by="year",
#                     all=TRUE)
# colnames(SoilBD_gcc) <- c("year","Observed","APSIM")
# 
# SoilBD_gcc_piv <- pivot_longer(SoilBD_gcc, c(-year),
#                names_to = "source",
#                values_to = "bd_val")

N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
                    APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                    by="date",
                    all=TRUE)
colnames(N2O_ghaday) <- c("date","Observed","APSIM")

N2O_ghaday_piv <- pivot_longer(N2O_ghaday, c(-date),
                               names_to = "source",
                               values_to = "n2o_val")

N2O_ghayr <- APSIMGN_ann_gha
colnames(N2O_ghayr) <- c("year","APSIM")

N2O_ghayr_piv <- pivot_longer(N2O_ghayr, c(-year),
                               names_to = "source",
                               values_to = "n2o_val")


#**********************************************************************

# calculate mean differences between observed and modeled results

Sorghum_obsmod_diff_Mgha <- sum(SorghumYld_Mgha[!is.na(SorghumYld_Mgha$Observed) &
                                              !is.na(SorghumYld_Mgha$APSIM),"Observed"] -
                                SorghumYld_Mgha[!is.na(SorghumYld_Mgha$Observed) &
                                                !is.na(SorghumYld_Mgha$APSIM),"APSIM"])
Cotton_obsmod_diff_Mgha <- sum(CottonYld_Mgha[!is.na(CottonYld_Mgha$Observed &
                                                     CottonYld_Mgha$APSIM),"Observed"] -
                                  CottonYld_Mgha[!is.na(CottonYld_Mgha$Observed &
                                                       CottonYld_Mgha$APSIM),"APSIM"])
SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                     Cstock_Mgha$APSIM),"Observed"] -
                                Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                       Cstock_Mgha$APSIM),"APSIM"])
SoilT_obsmod_diff_Mgha <- mean(SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                            !is.na(SoilTemp_C$APSIM),"Observed"] -
                                 SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                              !is.na(SoilTemp_C$APSIM),"APSIM"])
# SoilM_obsmod_diff_Mgha <- mean(SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed &
#                                                       SoilMoist_VSM$APSIM),"Observed"] -
#                                  SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed &
#                                                         SoilMoist_VSM$APSIM),"APSIM"])
# N2O_obsmod_diff_gha <- sum(N2O_ghaday[!is.na(N2O_ghaday$Observed) &
#                                          !is.na(N2O_ghaday$APSIM),"Observed"] -
#                               N2O_ghaday[!is.na(N2O_ghaday$Observed) &
#                                            !is.na(N2O_ghaday$APSIM),"APSIM"])

SOC_obsmod_diff_Mgha_nooutliers <- NA