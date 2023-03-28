#title: "10_Model_Ensemble_Results-by_scenario"
#author: "Ellen Maas"
#date: "8/30/2022"
#output: html_document
#description: "Runs all output of interest for all models."

suppressMessages({
  
print("Starting 10_Model_Ensemble_Results-by_scenario.R")

library(apsimx)
library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)

  # read in calibration stats
  calib_summary_raw <- read.csv(paste0(results_path,"Calibration_summary.csv"))
  calib_summary_df <- calib_summary_raw[calib_summary_raw$Scenario_Name==scenario_name]
    
  
# 9-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]green, [4]light blue, [5]grey,
#[6]pink, [7]red, [8]orange, [9]yellow

  if(mgmt_scenario_grp!=6) {
  
# merge observed and modeled data
## use ens_ (ensemble) prefix to distinguish these from the "MaizeYld_Mgha" etc.
## files in each model's Results files.
ens_MaizeYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield","sd_yield")],
                             APSIMY_Mgha[APSIMY_Mgha$MaizeYield_Mgha != 0,
                                         c("year","MaizeYield_Mgha")],
                             by="year",
                             all=TRUE),
                       DayY_Mgha[DayY_Mgha$crop=="Maize",c("year","yield")],
                       by="year",
                       all=TRUE)
colnames(ens_MaizeYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent")

ens_MaizeYld_Mgha_piv <- pivot_longer(ens_MaizeYld_Mgha, c(-year,-Obs_sd),
               names_to = "Model",
               values_to = "yield_val")

ens_MaizeYld_Mgha_piv <- ens_MaizeYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))

#
ens_SoyYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield","sd_yield")],
                           APSIMY_Mgha[APSIMY_Mgha$SoyYield_Mgha != 0,
                                       c("year","SoyYield_Mgha")],
                           by="year",
                           all=TRUE),
                     DayY_Mgha[DayY_Mgha$crop=="Soybean",c("year","yield")],
                     by="year",
                     all=TRUE)
colnames(ens_SoyYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent")

ens_SoyYld_Mgha_piv <- pivot_longer(ens_SoyYld_Mgha, c(-year,-Obs_sd),
               names_to = "Model",
               values_to = "yield_val")

ens_SoyYld_Mgha_piv <- ens_SoyYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))

#
ens_WheatYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield","sd_yield")],
                             APSIMY_Mgha[APSIMY_Mgha$WheatYield_Mgha != 0,
                                         c("year","WheatYield_Mgha")],
                             by="year",
                             all=TRUE),
                       DayY_Mgha[DayY_Mgha$crop=="Wheat",c("year","yield")],
                       by="year",
                       all=TRUE)
colnames(ens_WheatYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent")

ens_WheatYld_Mgha_piv <- pivot_longer(ens_WheatYld_Mgha, c(-year,-Obs_sd),
               names_to = "Model",
               values_to = "yield_val")

ens_WheatYld_Mgha_piv <- ens_WheatYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))

##
ens_Cstock_Mgha <- merge(merge(merge(merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
                                       APSIMC_Mgha,
                                       by="year",
                                       all=TRUE),
                                DayC_Mgha[,c("year","base")],
                                by="year",
                                all=TRUE),
                           RothCC_Mgha[,c("year","ModC")],
                           by="year",
                           all=TRUE),
                       millC_Mgha_25cm,
                       by="year",
                       all=TRUE)

colnames(ens_Cstock_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent","RothC","Millennial")

ens_Cstock_Mgha_piv <-  pivot_longer(ens_Cstock_Mgha, c(-year,-Obs_sd),
               names_to = "Model",
               values_to = "C_val")

### remove sd's from models, since it's only for observations
ens_Cstock_Mgha_piv_adj <- ens_Cstock_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))

## N2O

ens_N2O_ghaday <- merge(merge(ObsGas[,c("date","N2O_N")],
                          APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                          by="date",
                          all=TRUE),
                    DayGN_ghaday[,c("date","N2O_gNhad")],
                    by="date",
                    all=TRUE)
colnames(ens_N2O_ghaday) <- c("date","Observed","APSIM","Daycent")

ens_N2O_ghaday_piv <- pivot_longer(ens_N2O_ghaday, c(-date),
                               names_to = "Model",
                               values_to = "n2o_val")

ens_N2O_cum_gha <- merge(APSIMGN_cum_gha[,c("date","N2O_gha")],
                    DayGN_cum_gha[,c("date","N2O_gha")],
                    by="date",
                  all=TRUE) 
colnames(ens_N2O_cum_gha) <- c("date","APSIM","Daycent")

ens_N2O_cum_kgha <- ens_N2O_cum_gha %>%
  mutate(APSIM=APSIM/1000,
         Daycent=Daycent/1000)

ens_N2O_cum_kgha_piv <- pivot_longer(ens_N2O_cum_kgha, c(-date),
                               names_to = "Model",
                               values_to = "n2o_val")


## CH4

ens_CH4_ghaday <- merge(ObsGas[,c("date","CH4_C")],
                          DayGM_ghaday[,c("date","CH4_net_gChad")],
                    by="date",
                    all=TRUE)
colnames(ens_CH4_ghaday) <- c("date","Observed","Daycent")

ens_CH4_ghaday_piv <- pivot_longer(ens_CH4_ghaday, c(-date),
                               names_to = "Model",
                               values_to = "ch4_val")

ens_CH4_cum_kgha <- ens_CH4_ghaday %>%
  mutate(Daycent = cumsum(Daycent)/1000)

ens_CH4_cum_kgha_piv <- pivot_longer(ens_CH4_cum_kgha, c(-date,-Observed),
                               names_to = "Model",
                               values_to = "ch4_val")



# Calibration temporal graphs ---------------------------------------------

if(clim_scenario_num == 1 & mgmt_scenario_grp %in% calib_mgmt_scenario_grps) {
  
  ## Maize
MYfit_APSIM <- coef(lm(APSIM ~ year, 
                       data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,]))
MYfit_Daycent <- coef(lm(Daycent ~ year, 
                         data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,]))
MYfit_Observed <- coef(lm(Observed ~ year, 
                         data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,]))

gMY_calib <- ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Maize Yield Calibration"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
  geom_abline(intercept=MYfit_Daycent[1], slope=MYfit_Daycent[2], color="#0072B2") +
  geom_abline(intercept=MYfit_Observed[1], slope=MYfit_Observed[2], color="#000000") +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                     values=cbPalette9[c(8,2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMY_calib

## Soybean

SYfit_APSIM <- coef(lm(APSIM ~ year, 
                       data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,]))
SYfit_Daycent <- coef(lm(Daycent ~ year, 
                         data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,]))
SYfit_Observed <- coef(lm(Observed ~ year, 
                          data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,]))

gSY_calib <- ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soybean Yield Calibration"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color="orange") +
  geom_abline(intercept=SYfit_Daycent[1], slope=SYfit_Daycent[2], color="#0072B2") +
  geom_abline(intercept=SYfit_Observed[1], slope=SYfit_Observed[2], color="#000000") +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                     values=cbPalette9[c(8,2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY_calib

# Wheat

WYfit_APSIM <- coef(lm(APSIM ~ year, 
                       data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,]))
WYfit_Daycent <- coef(lm(Daycent ~ year, 
                         data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,]))
WYfit_Observed <- coef(lm(Observed ~ year, 
                          data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,]))

gWY_calib <- ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Wheat Yield Calibration"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=WYfit_APSIM[1], slope=WYfit_APSIM[2], color="orange") +
  geom_abline(intercept=WYfit_Daycent[1], slope=WYfit_Daycent[2], color="#0072B2") +
  geom_abline(intercept=WYfit_Observed[1], slope=WYfit_Observed[2], color="#000000") +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) +  # Width of the error bars
  scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                     values=cbPalette9[c(8,2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWY_calib

## SOC

# if(mgmt_scenario_num==3) {
#   Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range &
#                                                                 ens_Cstock_Mgha$year!=1998,]))
#   Cfit_Obs_noout <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range &
#                                                                       ens_Cstock_Mgha$year!=1998 &
#                                                                       !(ens_Cstock_Mgha$Observed %in% ObsC_outliers),]))
#   ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range &
#                                              ens_Cstock_Mgha_piv_adj$year!=1998,]
# } else {
#   Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range,]))
#   Cfit_Obs_noout <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range &
#                                                                       !(ens_Cstock_Mgha$Observed %in% ObsC_outliers),]))
#   ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,]
# }

  Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range,]))
  Cfit_Obs_noout <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range &
                                                                      !(ens_Cstock_Mgha$Observed %in% ObsC_outliers),]))
  ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,]

gC_calib <- ens_Cstock_df[ens_Cstock_df$year %in% experiment_year_range &
                      ens_Cstock_df$Model == "Observed",] %>%
  ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
  geom_point(show.legend=TRUE) +
  geom_point(data=ens_Cstock_df[ens_Cstock_df$year %in% experiment_year_range &
                                 ens_Cstock_df$Model != "Observed",],
            aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  geom_abline(intercept=Cfit_Obs_noout[1], slope=Cfit_Obs_noout[2], color="black", linetype=2) +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon Calibration"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                     values=cbPalette9[c(8,2,6,1,3)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC_calib 

ggsave(filename=paste0(results_path,"pub_Ensemble_Maize_calibration_",scenario_name,".jpg"),
       plot=gMY_calib, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_Ensemble_Soybean_calibration_",scenario_name,".jpg"),
       plot=gSY_calib, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_Ensemble_Wheat_calibration_",scenario_name,".jpg"),
       plot=gWY_calib, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_Ensemble_SOC_calibration_",scenario_name,".jpg"),
       plot=gC_calib, width=9, height=6, dpi=300)

}



# Future temporal graphs --------------------------------------------------



MYfit_APSIM <- coef(lm(APSIM ~ year, 
                       data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year>=experiment_end_year,]))
MYfit_Daycent <- coef(lm(Daycent ~ year, 
                         data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year>=experiment_end_year,]))

gMY <- ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                             ens_MaizeYld_Mgha_piv$year>=experiment_end_year,] %>%
ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Future Maize Yield: ",scenario_descriptor)) +
  geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
  geom_abline(intercept=MYfit_Daycent[1], slope=MYfit_Daycent[2], color="#0072B2") +
  scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMY

#
SYfit_APSIM <- coef(lm(APSIM ~ year, 
                       data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year>=experiment_end_year,]))
SYfit_Daycent <- coef(lm(Daycent ~ year, 
                         data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year>=experiment_end_year,]))

gSY <- ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                             ens_SoyYld_Mgha_piv$year>=experiment_end_year,] %>%
ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Future Soybean Yield: ",scenario_descriptor)) +
  geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color="orange") +
  geom_abline(intercept=SYfit_Daycent[1], slope=SYfit_Daycent[2], color="#0072B2") +
  scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY

#
WYfit_APSIM <- coef(lm(APSIM ~ year, 
                       data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year>=experiment_end_year,]))
WYfit_Daycent <- coef(lm(Daycent ~ year, 
                         data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year>=experiment_end_year,]))

gWY <- ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                               ens_WheatYld_Mgha_piv$year>=experiment_end_year,] %>%
ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Future Wheat Yield: ",scenario_descriptor)) +
  geom_abline(intercept=WYfit_APSIM[1], slope=WYfit_APSIM[2], color="orange") +
  geom_abline(intercept=WYfit_Daycent[1], slope=WYfit_Daycent[2], color="#0072B2") +
  scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWY

if(mgmt_scenario_num==3) {
  Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year >= experiment_start_year &
                                                           ens_Cstock_Mgha$year!=1998,]))
  ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year>=experiment_start_year &
                            ens_Cstock_Mgha_piv_adj$year!=1998,]
} else {
  Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year >= experiment_start_year,]))
  ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year>=experiment_start_year,]
}

gC <- ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                       ens_Cstock_df$Model == "Observed",] %>%
  ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
  geom_point(show.legend=TRUE) +
  geom_line(data=ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                       ens_Cstock_df$Model != "Observed",],
            aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                     values=cbPalette9[c(8,2,6,1,3)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 

gNG <- ens_N2O_cum_kgha_piv %>%
  ggplot(aes(x=date, y=n2o_val, color=Model)) +
  geom_line(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
  ylim(0,100) +
  ggtitle(paste(site_name,"N2O Emissions: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG


gMG <- ens_CH4_cum_kgha_piv %>%
  ggplot(aes(x=date, y=ch4_val, color=Model)) +
  geom_line(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('CH'[4]*' Net Emissions (kg ha ' ^-1*')')) +
  ylim(-85,0) +
  ggtitle(paste(site_name,expression('CH'[4]*' Emissions: '),scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMG

  } else { # mgmt_scenario_grp == 6
    
    # merge observed and modeled data
    ## use ens_ (ensemble) prefix to distinguish these from the "MaizeYld_Mgha" etc.
    ## files in each model's Results files.
    ens_MaizeYld_Mgha <- merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield")],
                                     APSIMY_Mgha[APSIMY_Mgha$MaizeYield_Mgha != 0,
                                                 c("year","MaizeYield_Mgha")],
                                     by="year",
                                     all=TRUE)
    colnames(ens_MaizeYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_MaizeYld_Mgha_piv <- pivot_longer(ens_MaizeYld_Mgha, c(-year),
                                          names_to = "Model",
                                          values_to = "yield_val")
    
    #
    ens_SoyYld_Mgha <- merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield")],
                                   APSIMY_Mgha[APSIMY_Mgha$SoyYield_Mgha != 0,
                                               c("year","SoyYield_Mgha")],
                                   by="year",
                                   all=TRUE)
    colnames(ens_SoyYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_SoyYld_Mgha_piv <- pivot_longer(ens_SoyYld_Mgha, c(-year),
                                        names_to = "Model",
                                        values_to = "yield_val")
    
    #
    ens_WheatYld_Mgha <- merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield")],
                                     APSIMY_Mgha[APSIMY_Mgha$WheatYield_Mgha != 0,
                                                 c("year","WheatYield_Mgha")],
                                     by="year",
                                     all=TRUE)
    colnames(ens_WheatYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_WheatYld_Mgha_piv <- pivot_longer(ens_WheatYld_Mgha, c(-year),
                                          names_to = "Model",
                                          values_to = "yield_val")
    
    ##
    ens_Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
                                               APSIMC_Mgha,
                                               by="year",
                                               all=TRUE)
    
    colnames(ens_Cstock_Mgha) <- c("year","Observed","APSIM")
    
    ens_Cstock_Mgha_piv <-  pivot_longer(ens_Cstock_Mgha, c(-year),
                                         names_to = "Model",
                                         values_to = "C_val")
    
    ens_Cstock_Mgha_piv_adj <- ens_Cstock_Mgha_piv
    
    ## N2O
    
    ens_N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
                                  APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                                  by="date",
                                  all=TRUE)
    colnames(ens_N2O_ghaday) <- c("date","Observed","APSIM")
    
    ens_N2O_ghaday_piv <- pivot_longer(ens_N2O_ghaday, c(-date),
                                       names_to = "Model",
                                       values_to = "n2o_val")
    
    ens_N2O_cum_gha <- APSIMGN_cum_gha[,c("date","N2O_gha")]
    colnames(ens_N2O_cum_gha) <- c("date","APSIM")
    
    ens_N2O_cum_kgha <- ens_N2O_cum_gha %>%
      mutate(APSIM=APSIM/1000)
    
    ens_N2O_cum_kgha_piv <- pivot_longer(ens_N2O_cum_kgha, c(-date),
                                         names_to = "Model",
                                         values_to = "n2o_val")
    
    
    # Temporal graphs
    
    
    MYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year>=experiment_end_year,]))

    gMY <- ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Maize Yield: ",scenario_descriptor)) +
      geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=cbPalette9[c(8,1)]) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gMY
    
    #
    SYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year>=experiment_end_year,]))

    gSY <- ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Soybean Yield: ",scenario_descriptor)) +
      geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color="orange") +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=cbPalette9[c(8,1)]) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gSY
    
    #
    WYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year>=experiment_end_year,]))

    gWY <- ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Wheat Yield: ",scenario_descriptor)) +
      geom_abline(intercept=WYfit_APSIM[1], slope=WYfit_APSIM[2], color="orange") +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=cbPalette9[c(8,1)]) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gWY
    
      Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year >= experiment_start_year,]))
      ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year>=experiment_start_year,]

    gC <- ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                          ens_Cstock_df$Model == "Observed",] %>%
      ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
      geom_point(show.legend=TRUE) +
      geom_line(data=ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                                     ens_Cstock_df$Model != "Observed",],
                aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
      ggtitle(paste(site_name,"Soil Organic Carbon: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=cbPalette9[c(8,1)]) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gC 
    
    gNG <- ens_N2O_cum_kgha_piv %>%
      ggplot(aes(x=date, y=n2o_val, color=Model)) +
      geom_line(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
      ggtitle(paste(site_name,"N2O Emissions: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM"),
                         values=cbPalette9[c(8)]) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gNG
    
}

ggsave(filename=paste0(results_path,"Ensemble_Maize_comparison_",scenario_name,".jpg"),
       plot=gMY, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"Ensemble_Soybean_comparison_",scenario_name,".jpg"),
       plot=gSY, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"Ensemble_Wheat_comparison_",scenario_name,".jpg"),
       plot=gWY, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"Ensemble_SOC_comparison_",scenario_name,".jpg"),
       plot=gC, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"Ensemble_N2O_cum_comparison_",scenario_name,".jpg"),
       plot=gNG, width=9, height=6, dpi=300)
if(mgmt_scenario_grp!=6) {
ggsave(filename=paste0(results_path,"Ensemble_CH4_cum_comparison_",scenario_name,".jpg"),
       plot=gMG, width=9, height=6, dpi=300)
}

}) # end suppressMessages

