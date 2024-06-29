#######################################
# File: "9_Results_LDNDC-setup_KBS.R"
# Author: "Ellen Maas"
# Date: "3/24/2023"
# Description: Imports results from LDNDC output. End date for scenario
# is always the maximum (such as 2100), so results will have to be limited
# to the end_fut_period_year for specific time-frame analysis.
#
#######################################
# Audit Log
# 3/24/2023: Created script
#
#######################################
print(paste0("Starting 9_Results_LDNDC-setup_.R"))

library(magrittr)
library(lubridate)
library(tidyverse)

#**********************************************************************

# import ------------------------------------------------------------------

# dir.create(paste0(dndc_path,site_name,"_output/"))
output_path <- paste0(dndc_path,site_name,"_output/")

## soil moisture

LDNDC_soil_water_day_raw <- fread(paste0(output_path,"soil_water_daily_",scenario_name2,".csv")) %>%
  mutate(year=year(datetime))

LDNDC_soil_water_day <- LDNDC_soil_water_day_raw[LDNDC_soil_water_day_raw$year <= end_fut_period_year
                                                 ,c("datetime","year","prec[mm]","evapot[mm]",
                                                    "soilwater_5cm[%]","soilwater_10cm[%]",
                                                    "soilwater_15cm[%]","soilwater_20cm[%]")] %>%
  mutate(date=as.Date(datetime),
         dayofyear=yday(date),
         prec_mm=`prec[mm]`,
         pot_evap_mm=`evapot[mm]`,
         soilwater_5cm=`soilwater_5cm[%]`,
         soilwater_10cm=`soilwater_10cm[%]`,
         soilwater_15cm=`soilwater_15cm[%]`,
         soilwater_20cm=`soilwater_20cm[%]`) %>%
  select(date,year,dayofyear,prec_mm,pot_evap_mm,soilwater_5cm,soilwater_10cm,
         soilwater_15cm,soilwater_20cm)

## harvest and biomass

# LDNDC_physiology_ann_raw <- read.csv(paste0(output_path,"physiology_yearly_",scenario_name,".csv"))
# 
# LDNDC_physiology_ann <- LDNDC_physiology_ann_raw[,c("datetime","species","DW_fru.kgDWm.2.",
#                                             "DW_above.kgDWm.2.")] %>%
#   mutate(date=as.Date(datetime),
#          year=year(date),
#          crop=ifelse(species=="FOCO","Maize",
#               ifelse(species=="SOYB","Soybean",
#               ifelse(species=="WIWH","Wheat","Error"))),
#          grain_yield_kgm2=`DW_fru.kgDWm.2.`,
#          ag_biomass_kgm2=`DW_above.kgDWm.2.`) %>%
#   select(date,year,crop,grain_yield_kgm2,ag_biomass_kgm2)

LDNDC_harvest_raw <- fread(paste0(output_path,"harvest_",scenario_name2,".csv")) %>%
  mutate(year=year(datetime),
         date=date(datetime)-1, # harvest is actually one day earlier
         crop=ifelse(species=="FOCO","Maize",
                     ifelse(species=="SOYB","Soybean",
                            ifelse(species=="WIWH","Wheat",
                                   ifelse(species=="COTT", "Cotton",
                                          ifelse(species=="SRAPE", "Rapeseed", # These cover crops are needed to prevent error row in df
                                                 ifelse(species=="COWP", "Cowpea",
                                                        ifelse(species=="WINTERRYE", "Winter Rye", "Error"))))))))
                                   

LDNDC_harvest <- LDNDC_harvest_raw[LDNDC_harvest_raw$year <= end_fut_period_year,] 

LDNDC_physiology_daily_raw <- fread(paste0(output_path,"physiology_daily_",scenario_name2,".csv")) %>%
  mutate(date=date(datetime),
         year=year(date))%>%
  mutate(NPP_gCm2 = (`dC_fol_grow[kgCm-2]` + `dC_fru_grow[kgCm-2]` +
                  `dC_frt_grow[kgCm-2]` + `dC_lst_grow[kgCm-2]`)*1000) # convert to grams

# Join with harvest data for the harvest date, when the maximum values
# for yield, biomass, etc. are in physiology data
LDNDC_physiology_day <- merge(LDNDC_harvest,
                              LDNDC_physiology_daily_raw[LDNDC_physiology_daily_raw$year <= end_fut_period_year
                                                         ,c("date","species","DW_fru[kgDWm-2]",
                                                            "DW_above[kgDWm-2]")],
                              by=c("date","species")) %>%
  group_by(year,crop) %>%
  summarize(grain_yield_kgm2=max(`DW_fru[kgDWm-2]`),
            ag_biomass_kgm2=max(`DW_above[kgDWm-2]`),
            grain_yield_kgha=grain_yield_kgm2*10000,
            ag_biomass_kgha=ag_biomass_kgm2*10000,
            grain_yield_Mgha=grain_yield_kgm2*10,
            ag_biomass_Mgha=ag_biomass_kgm2*10)


# AD for cover crops, ldndc reports harvest amounts, so exclude those
LDNDC_physiology_daily_raw_no_cover_crops<-LDNDC_physiology_daily_raw%>%
  filter(species =="FOCO" | # ldndc reports this when there's more than 1 crop
           species == "SOYB" |
           species == "COTT" |
           species == "WIWH")


LDNDC_physiology_day <-LDNDC_physiology_daily_raw_no_cover_crops[LDNDC_physiology_daily_raw_no_cover_crops$year <= end_fut_period_year
                                                         ,c("date","species","DW_fru[kgDWm-2]",
                                                            "DW_above[kgDWm-2]", "NPP_gCm2")]%>%
  mutate(year = year(date),
         crop=ifelse(species=="FOCO","MaizeYld_Mgha",
              ifelse(species=="SOYB","SoybeanYld_Mgha",
                     ifelse(species=="WIWH","WheatYld_Mgha",
                            ifelse(species=="COTT", "CottonYld_Mgha", "Error")))))


LDNDC_physiology_ann <-LDNDC_physiology_daily_raw_no_cover_crops[LDNDC_physiology_daily_raw_no_cover_crops$year <= end_fut_period_year
                                                                 ,c("date","species","DW_fru[kgDWm-2]",
                                                                    "DW_above[kgDWm-2]")]%>%
  mutate(year = year(date),
         crop=ifelse(species=="FOCO","MaizeYld_Mgha",
                     ifelse(species=="SOYB","SoybeanYld_Mgha",
                            ifelse(species=="WIWH","WheatYld_Mgha",
                                   ifelse(species=="COTT", "CottonYld_Mgha", "Error")))))%>%
  group_by(year, crop) %>%
  summarize(grain_yield_kgm2=max(`DW_fru[kgDWm-2]`),
            ag_biomass_kgm2=max(`DW_above[kgDWm-2]` ),
            grain_yield_kgha=grain_yield_kgm2*10000,
            ag_biomass_kgha=ag_biomass_kgm2*10000,
            grain_yield_Mgha=grain_yield_kgm2*10,
            ag_biomass_Mgha=ag_biomass_kgm2*10)

### pivot to wider format to match APSIM and Daycent
LDNDC_yield <- pivot_wider(LDNDC_physiology_ann,names_from=crop,
                           values_from=grain_yield_Mgha)


# LDNDC_soil_chem_daily_raw <- fread(paste0(output_path,"soil_chem_daily_",scenario_name2,".csv")) %>%
#   mutate(year=year(datetime))
# colnames(LDNDC_soil_chem_daily_raw)



## soil carbon, annual ghg, no3

LDNDC_soil_chem_ann_raw <- fread(paste0(output_path,"soil_chem_yearly_",scenario_name2,".csv")) %>%
  mutate(year=year(datetime))

LDNDC_soil_chem_ann <- LDNDC_soil_chem_ann_raw[LDNDC_soil_chem_ann_raw$year <= end_fut_period_year
                                               ,c("datetime","year","aC_ch4_emis[kgCha-1]",
                                               "aC_co2_emis_auto[kgCha-1]","aC_co2_emis_hetero[kgCha-1]",
                                               "aN_n2o_emis[kgNha-1]","aN_no3_leach[kgNha-1]",
                                               "aC_doc_leach[kgCha-1]",
                                               "C_soil_min[kgCha-1]","C_soil_min_20cm[kgCha-1]",
                                               "C_soil_min_30cm[kgCha-1]")] %>%
  mutate(date=as.Date(datetime),
         dayofyear=yday(date),
         ch4_kgha=`aC_ch4_emis[kgCha-1]`,
         n2o_kgha=`aN_n2o_emis[kgNha-1]`,
         no3_kgha=`aN_no3_leach[kgNha-1]`,
         doc_kgha=`aC_doc_leach[kgCha-1]`,
         orgC_prof_kgha=`C_soil_min[kgCha-1]`,
         orgC_20cm_kgha=`C_soil_min_20cm[kgCha-1]`,
         orgC_30cm_kgha=`C_soil_min_30cm[kgCha-1]`,
         co2_auto_kgha=`aC_co2_emis_auto[kgCha-1]`,
         co2_hetero_kgha=`aC_co2_emis_hetero[kgCha-1]`,
         ch4_gha=ch4_kgha*1000,
         orgC_prof_Mgha=orgC_prof_kgha/1000,
         orgC_20cm_Mgha=orgC_20cm_kgha/1000,
         orgC_30cm_Mgha=orgC_30cm_kgha/1000,
         orgC_25cm_Mgha=round((orgC_20cm_Mgha+orgC_30cm_Mgha)/2,1)
         ) 

## soil temperature

LDNDC_soil_temp_day_raw <- fread(paste0(output_path,"soil_temp_daily_",scenario_name2,".csv")) %>%
  mutate(year=year(datetime))

LDNDC_soil_temp_day <- LDNDC_soil_temp_day_raw[LDNDC_soil_temp_day_raw$year <= end_fut_period_year,] %>%
  mutate(date=as.Date(datetime),
         dayofyear=yday(date),
         s_temp_5cm=round(`temp_5cm[oC]`,1),
         s_temp_10cm=round(`temp_10cm[oC]`),
         s_temp_15cm=round(`temp_15cm[oC]`),
         s_temp_20cm=round(`temp_20cm[oC]`),
         s_temp_30cm=round(`temp_30cm[oC]`)) %>%
  select(date,year,dayofyear,s_temp_5cm,s_temp_10cm,s_temp_15cm,s_temp_20cm,s_temp_30cm)

## all daily GHG

LDNDC_soil_chem_day_raw <-  fread(paste0(output_path,"soil_chem_daily_",scenario_name2,".csv")) %>%
  mutate(year=year(datetime))

LDNDC_soil_chem_day <- LDNDC_soil_chem_day_raw[LDNDC_soil_chem_day_raw$year <= end_fut_period_year,
                                               c("datetime","year","dC_ch4_emis[kgCha-1]",
                                                  "dC_co2_emis_auto[kgCha-1]","dC_co2_emis_hetero[kgCha-1]",
                                                  "dN_n2o_emis[kgNha-1]","dN_no3_leach[kgNha-1]",
                                                  "dC_doc_leach[kgCha-1]")] %>%
  mutate(date=as.Date(datetime),
         dayofyear=yday(date),
         ch4_kgha=`dC_ch4_emis[kgCha-1]`,
         n2o_kgha=`dN_n2o_emis[kgNha-1]`,
         no3_kgha=`dN_no3_leach[kgNha-1]`,
         doc_kgha=`dC_doc_leach[kgCha-1]`,
         co2_auto_kgha=`dC_co2_emis_auto[kgCha-1]`,
         co2_hetero_kgha=`dC_co2_emis_hetero[kgCha-1]`
  ) 

## MeTrx model output

LDNDC_metrx_day_raw <- fread(paste0(output_path,"metrx-daily_",scenario_name2,".csv"))

colnames(LDNDC_metrx_day_raw)

# get bulk density to convert SOC % to kg/ha
# https://www.soilquality.org.au/factsheets/organic-carbon

# SOC stock in tonnes of carbon per hectare (tC/ha) = (soil organic carbon %) x (mass of soil in a given volume)
# 
# For example, a soil with a SOC of 1.3% (0.013) and a bulk density of 1.2 grams per cubic centimetre 
# (equivalent to 1.2 tonnes per cubic metre), would have SOC to a depth of 10 cm (0.1 m) per hectare (10 000 m2) of:
#   SOC %      BD(t/m3)   depth(m)    area ha
#   (0.013) x (1.2 x 0.1 x 10 000) = 15.6 tC/ha.

# gNATSGO bulk density in g/cm3

bulk_density<-filter(soil_df_L, lower_depth_cm==20)$bdfiod_value_avg # assume is g/cm3

LDNDC_metrx_day <- LDNDC_metrx_day_raw%>%
  # select(c(source, datetime, fe2_tot, fe3_tot, `o2[kgha-1]`,ph_soil_surface,`wfps[%]`,
  #          `soc_20cm[%]`, `soc_40cm[%]`)) %>%
  mutate(date=as.Date(datetime),
         year=year(date),
         wfps=`wfps[%]`,
         o2_kgha=`o2[kgha-1]`
         ) %>%
  # % SOC * bulk_Density kg/ha * depth cm (20) * tonnes to kg (1000) = C kgha-1
  mutate(`SOC_20cm[tonnesha-1]`= `soc_20cm[%]`*bulk_density*.2*10000,
         `SOC_20cm[gm2-1]`= `soc_20cm[%]`*bulk_density*.2*100,
         `SOC_40cm[tonnesha-1]`= `soc_40cm[%]`*bulk_density*.2*10000,
         `SOC_40cm[gm2-1]`= `soc_40cm[%]`*bulk_density*.2*100) %>% # check with Debjani
  select(date,year,source,fe2_tot,fe3_tot, o2_kgha,ph_soil_surface,wfps, `SOC_40cm[gm2-1]`)

#***********************2***********************************************

# construct dataframes of obs data ----------------------------------------

# yield
# LDNDCY_Mgha <- LDNDC_yield[,c("year","Maize","Soybean",
#                             "Wheat")] %>%

if(crop=="Rotation"){
  LDNDCY_Mgha<-select(LDNDC_yield, year, MaizeYld_Mgha, SoybeanYld_Mgha)
} else {
  LDNDCY_Mgha<-select(LDNDC_yield, year, eval(paste0(crop,'Yld_Mgha')))
}
# LDNDCY_Mgha <- select(LDNDC_yield, year, eval(crop)) %>%
#   group_by(year) %>%
#   summarize(Yield_Mgha=eval(crop))
  # summarize(Yield_Mgha=Maize,
  #         SoyYield_Mgha=Soybean,
  #         WheatYield_Mgha=Wheat)

# soil carbon - annual
LDNDCC_Mgha <- LDNDC_soil_chem_ann[,c("year","orgC_30cm_Mgha")] %>%
  mutate(TotalSOC_30cm_Mgha=round(orgC_30cm_Mgha,1)) %>%
  select(year,TotalSOC_30cm_Mgha)

## soil temperature - daily
LDNDCT_C <- LDNDC_soil_temp_day[,c("date","year","dayofyear","s_temp_20cm")] %>%
  mutate(SoilTemp_20cm_C=round(s_temp_20cm,1)) 

## volumetric soil moisture
LDNDCM_V <- LDNDC_soil_water_day[,c("date","year","dayofyear","soilwater_20cm")] %>%
  mutate(VolH2O_20cm=round(soilwater_20cm,0))

## bulk density
# LDNDCB_gcc <- LDNDC_out[,c("date","year","BulkDensity_gcc(1)")] %>%
#   mutate(BulkDensity_gcc=round(LDNDC_out$`BulkDensity_gcc(1)`,2))

## N2O emissions
LDNDCGN_ghaday <- LDNDC_soil_chem_day[,c("date","year","dayofyear","n2o_kgha")] %>%
  mutate(N2OEmissions_ghaday = round(n2o_kgha*1000,2),
         dayofyear = yday(date))%>%
  mutate(cum_N2O_gha = cumsum(N2OEmissions_ghaday)) %>%
  select(date,year,N2OEmissions_ghaday, cum_N2O_gha)
  

LDNDCGN_ann_gha <- LDNDCGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2OEmissions_ghaday))

### verify that adding daily emissions equals the annual given in
### LDNDC soil chem yearly file - it does
LDNDCGN2_ann_gha <- LDNDC_soil_chem_ann[,c("year","n2o_kgha")] %>%
  mutate(N2OEmissions_ghaday = round(n2o_kgha*1000,2))

LDNDCGN_cum_gha <- LDNDCGN_ghaday %>%
  mutate(cum_N2O_gha = cumsum(N2OEmissions_ghaday)) %>%
  select(date,year,cum_N2O_gha)

## CH4 emissions
LDNDCGM_ghaday <- LDNDC_soil_chem_day[,c("date","year","dayofyear","ch4_kgha")] %>%
  mutate(CH4Emissions_ghaday = round(ch4_kgha*1000,2),
         dayofyear = yday(date))%>%
  mutate(cum_CH4_gha = cumsum(CH4Emissions_ghaday)) %>%
  select(date,year,cum_CH4_gha, CH4Emissions_ghaday)

LDNDCGM_ann_gha <- LDNDCGM_ghaday %>%
  group_by(year) %>%
  summarize(CH4Emissions_ghayr=sum(CH4Emissions_ghaday))

### verify that adding daily emissions equals the annual given in
### LDNDC soil chem yearly file - it does
LDNDCGM2_ann_gha <- LDNDC_soil_chem_ann[,c("year","ch4_gha")] %>%
  mutate(CH4Emissions_ghaday = round(ch4_gha*1000,2))

LDNDCGM_cum_gha <- LDNDCGM_ghaday %>%
  mutate(cum_CH4_gha = cumsum(CH4Emissions_ghaday)) %>%
  select(date,year,cum_CH4_gha)

### test threshold for CH4 production:
### Fe3+ < METRX_FRAC_FE_CH4_PROD (0.3) * (Fe3+ + Fe2+)
# fe_tot <- 0.3 * (LDNDC_metrx_day$fe3_tot + LDNDC_metrx_day$fe2_tot)
# ch4_prod <- data.frame(date=LDNDC_metrx_day$date,
#                        year=LDNDC_metrx_day$year,
#                        production=ifelse(LDNDC_metrx_day$fe3_tot<fe_tot,TRUE,FALSE),
#                        wfps=LDNDC_metrx_day$wfps,
#                        fe3_tot=LDNDC_metrx_day$fe3_tot,
#                        metrx_frac_fe_ch4_prod=0.3,
#                        fe_tot=fe_tot,
#                        fe2_tot=LDNDC_metrx_day$fe2_tot,
#                        o2_kgha=LDNDC_metrx_day$o2_kgha,
#                        ph=LDNDC_metrx_day$ph_soil_surface)


# CO2

# head(LDNDC_soil_chem_day)

CO2_resp_ann<-LDNDC_soil_chem_day%>%
  group_by(year)%>%
  summarize(CO2_resp_gha=sum(co2_hetero_kgha*1000))

#**********************************************************************

model_name = 'LDNDC'

# write out results -------------------------------------------------------

# write out results for use later in ensemble results
head(LDNDCY_Mgha)
head(LDNDCC_Mgha)
head(LDNDCGM_ann_gha)
head(LDNDCGM_ann_gha)


# LDNDC_yield
# LDNDC_soil_chem_ann
# LDNDCY_Mgha
# LDNDCGN_ann_gha
# LDNDCGN2_ann_gha
# LDNDCGM_ann_gha
# LDNDCGM2_ann_gha
# CO2_resp_ann


output_annual_data <- 
  left_join(LDNDC_yield, LDNDC_soil_chem_ann, by=c("year")) %>%
  left_join(LDNDCGM_ann_gha, by=c("year")) %>%
  left_join(LDNDCGN_ann_gha, by=c("year"))%>%
  left_join(CO2_resp_ann, by=c("year"))%>%
  mutate(scenario_name = scenario_name2,
         climate_scenario_num = clim_scenario_num,
         model_name = model_name)%>%
  select(year, scenario_name, climate_scenario_num, model_name, grain_yield_kgm2:CO2_resp_gha)
  
colnames(output_annual_data)

# 
# colnames(output_annual_data) <- c("year",paste0(crop, "Yld_Mgha"),"SOC_Mgha","model_name",
#                                   "scenario_name","climate_scenario_num")
#                                   # "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

# head(LDNDCGN_ghaday)
# head(LDNDCGM_ghaday)


# LDNDC_soil_water_day
# LDNDC_physiology_day
# LDNDC_soil_chem_day
# LDNDC_soil_temp_day
# LDNDC_metrx_day
# LDNDCGN_ghaday
# LDNDCGN_cum_gha
# LDNDCGM_ghaday
# LDNDCGM_cum_gha
# ch4_prod
# LDNDC_physiology_day

output_daily_data<-left_join(LDNDCGN_ghaday, select(LDNDCGM_ghaday, -year), by=c("date"))%>%
  left_join(select(LDNDC_soil_water_day, -year, -dayofyear), by=c("date"))%>%
  left_join(select(LDNDC_physiology_day, -year), by=c("date"))%>%
  left_join(select(LDNDC_soil_chem_day, -year, -dayofyear), by=c("date"))%>%
  left_join(select(LDNDC_soil_temp_day, -year), by=c("date"))%>%
  left_join(select(LDNDC_metrx_day, -year), by=c("date"))%>%
  mutate(scenario_name = scenario_name2,
         climate_scenario_num = clim_scenario_num,
         model_name = model_name)%>%
  select(date, year, dayofyear, scenario_name, climate_scenario_num, model_name, N2OEmissions_ghaday:`SOC_40cm[gm2-1]`)

colnames(output_daily_data)
  

#                          # mgmt_scenario_grp,mgmt_scenario_opt)
# colnames(output_daily_data) <- c("date","year","dayofyear","N2O_emit_gha","N2O_cum_gha",
#                                  # "CH4_net_gha","CH4_cum_gha",
#                                  "model_name","scenario_name","climate_scenario_num")
#                                  # "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

fwrite(output_annual_data,file=ldndc_annual_out, col.names=T,row.names=F,sep=",",append=F)
fwrite(output_daily_data,file=ldndc_daily_out, col.names=T,row.names=F,sep=",",append=F)


# Read last lines of mylog.txt file, save, then delete
# this is because it's a 100MB file and only the last lines give useful info
# https://stackoverflow.com/questions/5596107/reading-the-last-n-lines-from-a-huge-text-file
ReadLastLines <- function(x,n,...){    
  con <- file(x)
  open(con)
  out <- scan(con,n,what="char(0)",sep="\n",quiet=TRUE,...)
  
  while(TRUE){
    tmp <- scan(con,1,what="char(0)",sep="\n",quiet=TRUE)
    if(length(tmp)==0) {close(con) ; break }
    out <- c(out[-1],tmp)
  }
  out
}

log<-ReadLastLines(paste0(getwd(), '/', dndc_path, "mylog.txt"),10)
writeLines(log, paste0(dndc_path, paste0("mylog_",scenario_name2,"_LDNDC.txt")))
unlink(file.path(dndc_path, "mylog.txt")) # delete log file

#**********************************************************************

# merge observed and modeled data -----------------------------------------
# 
# MaizeYld_Mgha <- right_join(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield")],
#                        LDNDCY_Mgha[!is.na(LDNDCY_Mgha$MaizeYield_Mgha),
#                                    c("year","MaizeYield_Mgha")],
#                        by="year") %>%
#   merge(HistY_Mgha[,c("year","maize_yield_mgha")],
#         by="year",
#         all=TRUE)
# colnames(MaizeYld_Mgha) <- c("year","Observed","LDNDC","Historical")
# 
# MaizeYld_Mgha_piv <- pivot_longer(MaizeYld_Mgha, c(-year),
#                                   names_to = "source",
#                                   values_to = "yield_val")
# 
# ##
# SoyYld_Mgha <- right_join(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield")],
#                      LDNDCY_Mgha[!is.na(LDNDCY_Mgha$SoyYield_Mgha),
#                                  c("year","SoyYield_Mgha")],
#                      by="year") %>%
#   merge(HistY_Mgha[HistY_Mgha$year>=1954,c("year","soybean_yield_mgha")],
#         by="year",
#         all=TRUE)
# colnames(SoyYld_Mgha) <- c("year","Observed","LDNDC","Historical")
# 
# SoyYld_Mgha_piv <- pivot_longer(SoyYld_Mgha, c(-year),
#                                 names_to = "source",
#                                 values_to = "yield_val")
# 
# ##
# WheatYld_Mgha <- right_join(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield")],
#                        LDNDCY_Mgha[!is.na(LDNDCY_Mgha$WheatYield_Mgha),
#                                    c("year","WheatYield_Mgha")],
#                        by="year") %>%
#   merge(HistY_Mgha[,c("year","wheat_yield_mgha")],
#         by="year",
#         all=TRUE)
# colnames(WheatYld_Mgha) <- c("year","Observed","LDNDC","Historical")
# 
# WheatYld_Mgha_piv <- pivot_longer(WheatYld_Mgha, c(-year),
#                                   names_to = "source",
#                                   values_to = "yield_val")
# 
# ##
# Cstock_Mgha <- full_join(ObsC_Mgha[,c("year","cstock")],
#                      LDNDCC_Mgha,
#                      by="year")
# colnames(Cstock_Mgha) <- c("year","Observed","LDNDC")
# 
# Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year),
#                                  names_to = "source",
#                                  values_to = "C_val")
# 
# ##
# SoilTemp_C <- merge(ObsTemp[,c("date","soil_temperature")],
#                     LDNDCT_C[,c("date","SoilTemp_20cm_C")],
#                     by="date",
#                     all=TRUE)
# colnames(SoilTemp_C) <- c("date","Observed","LDNDC")
# 
# SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
#                                names_to = "source",
#                                values_to = "temp_val") %>%
#   mutate(year=year(date))
# 
# ##
# SoilMoist_VSM <- merge(ObsVSM[,c("date","mean_VSM")],
#                        LDNDCM_V[,c("date","VolH2O_20cm")],
#                        by="date",
#                        all=TRUE)
# colnames(SoilMoist_VSM) <- c("date","Observed","LDNDC")
# 
# SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date),
#                                   names_to = "source",
#                                   values_to = "h2o_val") %>%
#   mutate(year=year(date))
# 
# ##
# # SoilBD_gcc <- merge(ObsBD[,c("year","mean_BD")],
# #                     LDNDCB_gcc[LDNDCB_gcc$date=="1996-01-01",c("year","BulkDensity_gcc")],
# #                     by="year",
# #                     all=TRUE)
# # colnames(SoilBD_gcc) <- c("year","Observed","LDNDC")
# # 
# # SoilBD_gcc_piv <- pivot_longer(SoilBD_gcc, c(-year),
# #                                names_to = "source",
# #                                values_to = "bd_val")
# 
# ##
# N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
#                     LDNDCGN_ghaday[,c("date","N2OEmissions_ghaday")],
#                     by="date",
#                     all=TRUE)
# colnames(N2O_ghaday) <- c("date","Observed","LDNDC")
# 
# N2O_ghaday_piv <- pivot_longer(N2O_ghaday, c(-date),
#                                names_to = "source",
#                                values_to = "n2o_val")
# 
# N2O_ghayr <- LDNDCGN_ann_gha
# colnames(N2O_ghayr) <- c("year","LDNDC")
# 
# N2O_ghayr_piv <- pivot_longer(N2O_ghayr, c(-year),
#                               names_to = "source",
#                               values_to = "n2o_val")
# 
# ##
# CH4_ghaday <- merge(ObsGas[,c("date","CH4_C")],
#                     LDNDCGM_ghaday[,c("date","CH4Emissions_ghaday")],
#                     by="date",
#                     all=TRUE)
# colnames(CH4_ghaday) <- c("date","Observed","LDNDC")
# 
# CH4_ghaday_piv <- pivot_longer(CH4_ghaday, c(-date),
#                                names_to = "source",
#                                values_to = "ch4_val")
# 
# CH4_ghayr <- LDNDCGM_ann_gha
# colnames(CH4_ghayr) <- c("year","LDNDC")
# 
# CH4_ghayr_piv <- pivot_longer(CH4_ghayr, c(-year),
#                               names_to = "source",
#                               values_to = "ch4_val")
# 
# #**********************************************************************
# 
# # calculate mean differences ----------------------------------------------
# 
# # calculate mean differences between observed and modeled results
# 
# Maize_obsmod_diff_Mgha <- sum(MaizeYld_Mgha[!is.na(MaizeYld_Mgha$Observed) &
#                                               !is.na(MaizeYld_Mgha$LDNDC),"Observed"] -
#                                 MaizeYld_Mgha[!is.na(MaizeYld_Mgha$Observed) &
#                                                 !is.na(MaizeYld_Mgha$LDNDC),"LDNDC"])
# Soybean_obsmod_diff_Mgha <- sum(SoyYld_Mgha[!is.na(SoyYld_Mgha$Observed &
#                                                      SoyYld_Mgha$LDNDC),"Observed"] -
#                                   SoyYld_Mgha[!is.na(SoyYld_Mgha$Observed &
#                                                        SoyYld_Mgha$LDNDC),"LDNDC"])
# Wheat_obsmod_diff_Mgha <- sum(WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed &
#                                                      WheatYld_Mgha$LDNDC),"Observed"] -
#                                 WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed &
#                                                        WheatYld_Mgha$LDNDC),"LDNDC"])
# SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
#                                                  Cstock_Mgha$LDNDC),"Observed"] -
#                               Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
#                                                    Cstock_Mgha$LDNDC),"LDNDC"])
# N2O_obsmod_diff_gha <- sum(N2O_ghaday[!is.na(N2O_ghaday$Observed) &
#                                         !is.na(N2O_ghaday$LDNDC),"Observed"] -
#                              N2O_ghaday[!is.na(N2O_ghaday$Observed) &
#                                           !is.na(N2O_ghaday$LDNDC),"LDNDC"])
# CH4_obsmod_diff_gha <- sum(CH4_ghaday[!is.na(CH4_ghaday$Observed) &
#                                         !is.na(CH4_ghaday$Daycent),"Observed"] -
#                              CH4_ghaday[!is.na(CH4_ghaday$Observed) &
#                                           !is.na(CH4_ghaday$Daycent),"Daycent"])

