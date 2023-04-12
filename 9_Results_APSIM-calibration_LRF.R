#title: "9_Results_APSIM-calibration"
#author: "Ellen Maas"
#date: "7/22/2022"
#output: html_document
#description: "Runs all graphs for the APSIM simulation at KBS, MI."

suppressMessages({
  
  print(paste0("Starting 9_Results_APSIM-calibration_",site_name,".R"))
  
library(magrittr)
library(tidyverse)
library(graphics)
library(ggplot2)

  #**********************************************************************
  
# Temporal graphs ---------------------------------------------------------


## experimental period

  ### cotton lint yield
  
  Cotton_this <- CottonYld_Mgha_piv[CottonYld_Mgha_piv$year %in% experiment_year_range,]
  
  CY_rmse_error <- pull(Cotton_this[Cotton_this$source=="Observed",],yield_val)-
    pull(Cotton_this[Cotton_this$source=="APSIM",],"yield_val")
  CY_rmse <- round(sqrt(mean(CY_rmse_error^2,na.rm=TRUE)),2)
  
gCY <- Cotton_this %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Cotton_this$year, na.rm=T),
           y=max(Cotton_this$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse))) +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Cotton Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCY

### sorghum grain yield

if(mgmt_scenario_grp != 7) {
Sorghum_this <- SorghumYld_Mgha_piv[SorghumYld_Mgha_piv$year %in% experiment_year_range,]

SY_rmse_error <- pull(Sorghum_this[Sorghum_this$source=="Observed",],yield_val)-
  pull(Sorghum_this[Sorghum_this$source=="APSIM",],"yield_val")
SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)

gSY <- Sorghum_this %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Sorghum_this$year, na.rm=T),
           y=max(Sorghum_this$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse))) +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Sorghum Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY
}

### cotton mid-August biomass yield

Cottonbio_this <- CottonBioYld_Mgha_piv[CottonBioYld_Mgha_piv$year %in% experiment_year_range,]

CBY_rmse_error <- pull(Cottonbio_this[Cottonbio_this$source=="Observed",],yield_val)-
  pull(Cottonbio_this[Cottonbio_this$source=="APSIM",],"yield_val")
CBY_rmse <- round(sqrt(mean(CBY_rmse_error^2,na.rm=TRUE)),2)

gCBY <- Cottonbio_this %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Cottonbio_this$year, na.rm=T),
           y=max(Cottonbio_this$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse))) +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('Cotton Biomass (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Cotton Biomass mid-August"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCBY

### sorghum mid-August biomass yield

if(mgmt_scenario_grp != 7) {
  Sorghumbio_this <- SorghumBioYld_Mgha_piv[SorghumBioYld_Mgha_piv$year %in% experiment_year_range,]
  
  SBY_rmse_error <- pull(Sorghumbio_this[Sorghumbio_this$source=="Observed",],yield_val)-
    pull(Sorghumbio_this[Sorghumbio_this$source=="APSIM",],"yield_val")
  SBY_rmse <- round(sqrt(mean(SBY_rmse_error^2,na.rm=TRUE)),2)
  
  gSBY <- Sorghumbio_this %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    annotate("text", # RMSE
             x=min(Sorghumbio_this$year, na.rm=T),
             y=max(Sorghumbio_this$yield_val, na.rm=T),
             hjust=0, family="serif", color="gray31",
             label=bquote("RMSE =" ~.(SY_rmse))) +
    geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                  width=.2) + # Width of the error bars
    xlab("Year") +
    ylab(expression('Sorghum Biomass (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Sorghum Biomass mid-August"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSBY
}

### SOC

  Cfit_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha[Cstock_Mgha$year %in% 2003:2021,]))#experiment_year_range,]))
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

  gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% 2003:2021,] %>%#experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
    geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                  width=.2) + # Width of the error bars
    xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ylim(3,12) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
    geom_abline(intercept=Cfit_APSIM[1], slope=Cfit_APSIM[2], color="orange") +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 


### SOC with spin-up

Cfith_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha[Cstock_Mgha$year %in% 2003:2021,]))#experiment_year_range,]))
Cfith_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gCh <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% 1987:2021,] %>%#experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=Cfith_Obs[1], slope=Cfith_Obs[2], color="black") +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ylim(0,12) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=Cfith_APSIM[1], slope=Cfith_APSIM[2], color="orange") +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCh 


### soil temp

gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='APSIM' 
                     & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
  ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed' 
                                 & SoilTemp_C_piv$year %in% ObsTemp$year,],
             aes(x=date, y=temp_val, color=source)) +
  xlab("Year") +
  ylab(expression('Soil temperature ( '*degree*C*")")) +
  ggtitle(paste0(site_name," Soil Temperature"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT

## Not using APSIM for RothC or Millennial, so no longer needed
# gT_calib <- SoilTemp_C_piv_calib[SoilTemp_C_piv_calib$source=='APSIM' 
#                                  & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
#   ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
#   geom_point() +
#   geom_point(data=SoilTemp_C_piv_calib[SoilTemp_C_piv_calib$source=='Observed' 
#                                        & SoilTemp_C_piv$year %in% ObsTemp$year,],
#              aes(x=date, y=temp_val, color=source)) +
#   xlab("Year") +
#   ylab(expression('Soil temperature ( '*degree*C*")")) +
#   ggtitle(paste0(site_name," Soil Temperature with ",soil_temp_bias,
#                  " deg C correction"),
#           paste0("Scenario: ",scenario_descriptor)) +
#   scale_color_manual(labels=c("APSIM","Observed"),
#                      values=cbPalette9[c(8,1)]) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.line = element_line(),
#         legend.position = "right",
#         legend.key = element_blank())
# 
# gT_calib

### soil moisture

gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM' 
                        & SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
  ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
  geom_point() +
  # geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed' 
  #                                   & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
  #            aes(x=date, y=h2o_val, color=source)) +
  xlab("Year") +
  ylab("Volumetric soil moisture") +
  ggtitle(paste0(site_name," Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM

gM_rain <- ggplot() +
  geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM' 
                        & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
             aes(x=date, y=h2o_val, color=source)) +
  geom_line(data=ObsWth[year(ObsWth$date) %in% year(ObsGas$date) &
                          ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * 2,
                        name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  ) +
  xlab("Year") +
  ylab("Volumetric soil moisture") +
  ggtitle(paste0(site_name," Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain"),
                     values=cbPalette9[c(8,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM_rain

gNG_rain <- ggplot() +
  geom_line(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                                  year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
            aes(x=date, y=n2o_val, color=source)) +
  geom_line(data=ObsWth[year(ObsWth$date) %in% year(ObsGas$date) &
                          ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
                                   year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * 2,
                        name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  ) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
               aes(x = date, y = 200,
                   xend = date, yend = 175),
               colour=cbPalette9[7],
               show.legend=F,
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.3, "cm"))
               # colour = "black" 
  ) + 
  xlab("Year") +
  ylab("Rain (mm)") +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Observed"),
                     values=cbPalette9[c(8,4,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

## Not using APSIM for RothC or Millennial, so no longer needed
# gM_calib <- SoilMoist_VSM_piv_calib[SoilMoist_VSM_piv_calib$source=='APSIM' 
#                                     & SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
#   ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
#   geom_point() +
#   geom_point(data=SoilMoist_VSM_piv_calib[SoilMoist_VSM_piv_calib$source=='Observed' 
#                                           & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
#              aes(x=date, y=h2o_val, color=source)) +
#   xlab("Year") +
#   ylab("Volumetric soil moisture") +
#   ggtitle(paste0(site_name," Volumetric soil moisture with ",soil_moist_bias,"% correction"),
#           paste0("Scenario: ",scenario_descriptor)) +
#   scale_color_manual(labels=c("APSIM","Observed"),
#                      values=cbPalette9[c(8,1)]) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.line = element_line(),
#         legend.position = "right",
#         legend.key = element_blank())
# 
# gM_calib

### N2O

gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                        year(N2O_ghaday_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  # geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
  #                                  year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
  #            aes(x=date, y=n2o_val, color=source)) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
               aes(x = date, y = 40,
                   xend = date, yend = 35),
               colour=cbPalette9[7],
               show.legend=F,
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.3, "cm"))
               # colour = "black" 
  ) + 
  xlab("Year") +
  ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Fertilizer"),
                     values=cbPalette9[c(8,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG

gNG_rain <- ggplot() +
  geom_line(data=ObsWth[year(ObsWth$date) %in% year(ObsGas$date) &
                     ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  geom_line(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                        year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
            aes(x=date, y=n2o_val, color=source)) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
                                   year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * 2,
                        name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  ) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
               aes(x = date, y = 200,
                   xend = date, yend = 175),
               colour=cbPalette9[7],
               show.legend=F,
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.3, "cm"))
               # colour = "black" 
  ) + 
  xlab("Year") +
  ylab("Rain (mm)") +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Observed"),
                     values=cbPalette9[c(8,4,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG_rain

gNG_rain5yr <- ggplot() +
  geom_line(data=ObsWth[year(ObsWth$date) %in% 2010:2015 &
                          ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  geom_line(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                                  year(N2O_ghaday_piv$date) %in% 2010:2015,],
            aes(x=date, y=n2o_val, color=source)) +
  # geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
  #                                  year(N2O_ghaday_piv$date) %in% 2010:2015,],
  #            aes(x=date, y=n2o_val, color=source)) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(trans = ~ .x * 1,
  #                       name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  # ) +
  geom_segment(data=Fert[Fert$treatment==treatment & year(Fert$date) %in% 2010:2015,],
               aes(x = date, y = 125,
                   xend = date, yend = 100),
               colour=cbPalette9[7],
               show.legend=F,
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.3, "cm"))
               # colour = "black"
  ) +
  xlab("Year") +
  ylab(expression('Rain (mm) and N '[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Fertilizer"),
                     values=cbPalette9[c(8,4,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG_rain5yr

gNG_5ghd <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                        year(N2O_ghaday_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
                                   year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source)) +
  # geom_segment(data=Fert[Fert$treatment==treatment & 
  #                          Fert$date %in% year(ObsGas$date) &
  #                          Fert$totalN_kgha>10,],
  #              aes(x = date, y = 200,
  #                  xend = date, yend = 175),
  #              colour=cbPalette9[7],
  #              show.legend=F,
  #              lineend = "round",
  #              linejoin = "round",
  #              arrow = arrow(length = unit(0.3, "cm"))
  #              # colour = "black" 
  # ) + 
  xlab("Year") +
  ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ylim(0,5) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions-limited display to 5 g/ha/day"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                     values=cbPalette9[c(8,1,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG_5ghd


ggsave(filename=paste0(results_path,"calib_Cotton_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gCY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Cotton_biomass_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gCBY,
       width=6, height=6, dpi=300)
if(mgmt_scenario_grp != 7) {
ggsave(filename=paste0(results_path,"calib_Sorghum_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gSY,
       width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Sorghum_biomass_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gSBY,
         width=6, height=6, dpi=300)
}
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gC,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_base_",scenario_name,"_APSIM.jpg"),plot=gCh,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gT,
       width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_calib_exp_",scenario_name,"_APSIM.jpg"),plot=gT_calib)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gM,
       width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_calib_exp_",scenario_name,"_APSIM.jpg"),plot=gM_calib)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gNG,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_rain5yr_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_rain5yr,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_5ghd_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_5ghd,
       width=9, height=6, dpi=300)


#**********************************************************************

# 1:1 graphs --------------------------------------------------------------


CYfit <- lm(APSIM ~ Observed, data = CottonYld_Mgha)
CYfit_coef <- coef(CYfit)
CYfit_r2 <- round(summary(CYfit)$r.squared,2)

CY_rmse_error <- CottonYld_Mgha$Observed-CottonYld_Mgha$APSIM
CY_rmse <- round(sqrt(mean(CY_rmse_error^2,na.rm=TRUE)),2)

gCY_121 <- CottonYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=CYfit_coef[1], slope=CYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(CYfit_coef[2],4))~"x" ~+ ~.(round(CYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(CYfit_r2))) +
  annotate("text", # RMSE
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*0.88,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse))) +
  ggtitle(bquote(.(site_name)~"Cotton Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gCY_121

if(mgmt_scenario_grp != 7) {
##
SYfit <- lm(APSIM ~ Observed, data = SorghumYld_Mgha)
SYfit_coef <- coef(SYfit)
SYfit_r2 <- round(summary(SYfit)$r.squared,2)

SY_rmse_error <- SorghumYld_Mgha$Observed-SorghumYld_Mgha$APSIM
SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)

gSY_121 <- SorghumYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(SYfit_coef[2],4))~"x" ~+ ~.(round(SYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(SYfit_r2))) +
  annotate("text", # RMSE
           x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse))) +
  ggtitle(bquote(.(site_name)~"Sorghum Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gSY_121
}

##  SOC
if(mgmt_scenario_grp==3) {
  Cfit <- lm(APSIM ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1998,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$APSIM
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
} else {
  Cfit <- lm(APSIM ~ Observed, data = Cstock_Mgha)
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$APSIM
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
}

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  ggtitle(bquote(.(site_name)~"SOC Stock (Mg C ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gC_121

##
Tfit <- lm(APSIM ~ Observed, data = SoilTemp_C)
Tfit_coef <- coef(Tfit)
Tfit_r2 <- round(summary(Tfit)$r.squared,2)

T_rmse_error <- SoilTemp_C$Observed-SoilTemp_C$APSIM
T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)

gT_121 <- SoilTemp_C %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Tfit_coef[2],4))~"x" ~+ ~.(round(Tfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Tfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*0.82,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(T_rmse))) +
  ggtitle(bquote(.(site_name)~"Soil Temperature ("*degree*"C)"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gT_121

# ## soil temperature with bias correction
# Tfit_calib <- lm(APSIM ~ Observed, data = SoilTemp_C_calib)
# Tfit_calib_coef <- coef(Tfit_calib)
# Tfit_calib_r2 <- round(summary(Tfit_calib)$r.squared,2)
# 
# T_calib_rmse_error <- SoilTemp_C_calib$Observed-SoilTemp_C_calib$APSIM
# T_calib_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)
# 
# gT_121_calib <- SoilTemp_C_calib %>%
#   ggplot(aes(x=Observed, y=APSIM,
#              xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
#              ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
#   geom_point() +
#   geom_abline() +
#   geom_abline(intercept=Tfit_calib_coef[1], slope=Tfit_calib_coef[2], color="blue") +
#   annotate("text", # line equation
#            x=min(SoilTemp_C_calib$Observed, SoilTemp_C_calib$APSIM, na.rm=T)*1.1,
#            y=max(SoilTemp_C_calib$Observed, SoilTemp_C_calib$APSIM, na.rm=T)*1,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("y =" ~.(round(Tfit_calib_coef[2],4))~"x" ~+ ~.(round(Tfit_calib_coef[1],4)))) +
#   annotate("text", # R^2
#            x=min(SoilTemp_C_calib$Observed, SoilTemp_C_calib$APSIM, na.rm=T)*1.1,
#            y=max(SoilTemp_C_calib$Observed, SoilTemp_C_calib$APSIM, na.rm=T)*0.92,
#            hjust=0, family="serif", color="gray31",
#            label=bquote(R^2 ~"=" ~.(Tfit_calib_r2))) +
#   annotate("text", # RMSE
#            x=min(SoilTemp_C_calib$Observed, SoilTemp_C_calib$APSIM, na.rm=T)*1.1,
#            y=max(SoilTemp_C_calib$Observed, SoilTemp_C_calib$APSIM, na.rm=T)*0.82,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("RMSE =" ~.(T_calib_rmse))) +
#   ggtitle(bquote(.(site_name)~"Soil temperature with"~.(soil_temp_bias)~""*degree*"C correction"),
#                  paste0("Scenario: ",scenario_descriptor)) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line = element_line())
# 
# gT_121_calib

# ##
# Mfit <- lm(APSIM ~ Observed, data = SoilMoist_VSM)
# Mfit_coef <- coef(Mfit)
# Mfit_r2 <- round(summary(Mfit)$r.squared,2)
# 
# M_rmse_error <- SoilMoist_VSM$Observed-SoilMoist_VSM$APSIM
# M_rmse <- round(sqrt(mean(M_rmse_error^2,na.rm=TRUE)),2)
# 
# gM_121 <- SoilMoist_VSM %>%
#   ggplot(aes(x=Observed, y=APSIM, 
#              xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T), 
#              ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
#   geom_point() +
#   geom_abline() +
#   geom_abline(intercept=Mfit_coef[1], slope=Mfit_coef[2], color="blue") +
#   annotate("text", # line equation
#            x=min(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1.1,
#            y=max(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("y =" ~.(round(Mfit_coef[2],4))~"x" ~+ ~.(round(Mfit_coef[1],4)))) +
#   annotate("text", # R^2
#            x=min(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1.1,
#            y=max(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*0.95,
#            hjust=0, family="serif", color="gray31",
#            label=bquote(R^2 ~"=" ~.(Mfit_r2))) +
#   annotate("text", # RMSE
#            x=min(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1.1,
#            y=max(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*0.89,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("RMSE =" ~.(M_rmse))) +
#   ggtitle(paste0(site_name," Volumetric Soil Moisture"),
#           paste0("Scenario: ",scenario_descriptor)) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line = element_line())
# 
# gM_121

## Not using APSIM for RothC or Millennial, so no longer needed
# ## soil moisture with bias correction
# Mfit_calib <- lm(APSIM ~ Observed, data = SoilMoist_VSM_calib)
# Mfit_calib_coef <- coef(Mfit_calib)
# Mfit_calib_r2 <- round(summary(Mfit_calib)$r.squared,2)
# 
# M_calib_rmse_error <- SoilMoist_VSM_calib$Observed-SoilMoist_VSM_calib$APSIM
# M_calib_rmse <- round(sqrt(mean(M_calib_rmse_error^2,na.rm=TRUE)),2)
# 
# gM_121_calib <- SoilMoist_VSM_calib %>%
#   ggplot(aes(x=Observed, y=APSIM, 
#              xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T), 
#              ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
#   geom_point() +
#   geom_abline() +
#   geom_abline(intercept=Mfit_calib_coef[1], slope=Mfit_calib_coef[2], color="blue") +
#   annotate("text", # line equation
#            x=min(SoilMoist_VSM_calib$Observed, SoilMoist_VSM_calib$APSIM, na.rm=T)*1.1,
#            y=max(SoilMoist_VSM_calib$Observed, SoilMoist_VSM_calib$APSIM, na.rm=T)*1,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("y =" ~.(round(Mfit_calib_coef[2],4))~"x" ~+ ~.(round(Mfit_calib_coef[1],4)))) +
#   annotate("text", # R^2
#            x=min(SoilMoist_VSM_calib$Observed, SoilMoist_VSM_calib$APSIM, na.rm=T)*1.1,
#            y=max(SoilMoist_VSM_calib$Observed, SoilMoist_VSM_calib$APSIM, na.rm=T)*0.95,
#            hjust=0, family="serif", color="gray31",
#            label=bquote(R^2 ~"=" ~.(Mfit_calib_r2))) +
#   annotate("text", # RMSE
#            x=min(SoilMoist_VSM_calib$Observed, SoilMoist_VSM_calib$APSIM, na.rm=T)*1.1,
#            y=max(SoilMoist_VSM_calib$Observed, SoilMoist_VSM_calib$APSIM, na.rm=T)*0.89,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("RMSE =" ~.(M_calib_rmse))) +
#   ggtitle(paste0(site_name, " Volumetric soil moisture with ",soil_moist_bias,"% correction"),
#           paste0("Scenario: ",scenario_descriptor)) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line = element_line())
# 
# gM_121_calib

# ##
# Nfit <- lm(APSIM ~ Observed, data = N2O_ghaday)
# Nfit_coef <- coef(Nfit)
# Nfit_r2 <- round(summary(Nfit)$r.squared,2)
# 
# N_rmse_error <- N2O_ghaday$Observed-N2O_ghaday$APSIM
# N_rmse <- round(sqrt(mean(N_rmse_error^2,na.rm=TRUE)),2)
# 
# gNG_121 <- N2O_ghaday[!is.na(N2O_ghaday$Observed),] %>%
#   ggplot(aes(x=Observed, y=APSIM,
#              xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
#              ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
#   xlim(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T), 
#        max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)) +
#   ylim(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T), 
#        max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)) +
#   geom_point() +
#   geom_abline() +
#   geom_abline(intercept=Nfit_coef[1], slope=Nfit_coef[2], color="blue") +
#   annotate("text", # line equation
#            x=abs(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1.1),
#            y=max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("y =" ~.(round(Nfit_coef[2],4))~"x" ~+ ~.(round(Nfit_coef[1],4)))) +
#   annotate("text", # R^2
#            x=abs(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1.1),
#            y=max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*0.95,
#            hjust=0, family="serif", color="gray31",
#            label=bquote(R^2 ~"=" ~.(Nfit_r2))) +
#   annotate("text", # RMSE
#            x=abs(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1.1),
#            y=max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*0.89,
#            hjust=0, family="serif", color="gray31",
#            label=bquote("RMSE =" ~.(N_rmse))) +
#   ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
#           paste0("Scenario: ",scenario_descriptor)) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line = element_line())
# 
# gNG_121

ggsave(filename=paste0(results_path,"calib_Cotton_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gCY_121, width=6, height=6, dpi=300)
if(mgmt_scenario_grp != 7) {
ggsave(filename=paste0(results_path,"calib_Sorghum_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gSY_121, width=6, height=6, dpi=300)
}
ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gT_121, width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_calib_",scenario_name,"_APSIM.jpg"),plot=gT_121_calib)
# ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_",scenario_name,"_APSIM.jpg"),
#        plot=gM_121, width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_calib_",scenario_name,"_APSIM.jpg"),plot=gM_121_calib)
# ggsave(filename=paste0(results_path,"calib_N2O_comparison_1to1_",scenario_name,"_APSIM.jpg"),
#        plot=gNG_121, width=6, height=6, dpi=300)


#**********************************************************************

# Log results -------------------------------------------------------------

# add this run's results to model log file and file collecting all final
# model runs
if(mgmt_scenario_grp != 7) {
calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                       clim_scenario_num,mgmt_scenario_num, scenario_name,
                       scenario_abbrev,
                       NA, NA, NA, NA, # Maize
                       NA,
                       NA, NA, NA, NA, # Soybean
                       NA,
                       NA, NA, NA, NA, # Wheat
                       NA,
                       Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse,
                       SOC_obsmod_diff_Mgha,
                       Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse,
                       NA, NA, NA, NA, # Moisture
                       NA, NA, NA, NA, # N2O
                       NA,
                       NA, NA, NA, NA, # CH4
                       NA,
                       CYfit_coef[2], CYfit_coef[1], CYfit_r2, CY_rmse,
                       Cotton_obsmod_diff_Mgha,
                       SYfit_coef[2], SYfit_coef[1], SYfit_r2, SY_rmse,
                       Sorghum_obsmod_diff_Mgha)
} else {
  calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                         clim_scenario_num,mgmt_scenario_num, scenario_name,
                         scenario_abbrev,
                         NA, NA, NA, NA, # Maize
                         NA,
                         NA, NA, NA, NA, # Soybean
                         NA,
                         NA, NA, NA, NA, # Wheat
                         NA,
                         Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse,
                         SOC_obsmod_diff_Mgha,
                         Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse,
                         NA, NA, NA, NA, # Moisture
                         NA, NA, NA, NA, # N2O
                         NA,
                         NA, NA, NA, NA, # CH4
                         NA,
                         CYfit_coef[2], CYfit_coef[1], CYfit_r2, CY_rmse,
                         Cotton_obsmod_diff_Mgha,
                         NA, NA, NA, NA, # Sorghum
                         NA)
}

source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)

}) # end suppressMessages