#title: "2_Calibrate_APSIM"
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

  
  Maize_this_piv <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year %in% experiment_year_range,]
  
  Maize_this <- MaizeYld_Mgha[MaizeYld_Mgha$year %in% experiment_year_range,]
  MYfit_time <- lm(APSIM ~ year, data = Maize_this)
  MYfit_coef_time <- coef(MYfit_time)
  MYfit_r2_time <- round(summary(MYfit_time)$r.squared,2)
  MY_rmse_error_time <- Maize_this$Observed-Maize_this$APSIM
  MY_rmse_time <- round(sqrt(mean(MY_rmse_error_time^2,na.rm=TRUE)),2)
  
  # #pull converts data frame to numeric array 
  # MY_rmse_error2 <- pull(Maize_this_piv[Maize_this_piv$source=="Observed",],yield_val)-
  #   pull(Maize_this_piv[Maize_this_piv$source=="APSIM",],"yield_val")
  # MY_rmse2 <- round(sqrt(mean(MY_rmse_error^2,na.rm=TRUE)),2)
  
gMY <- Maize_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Maize_this_piv$year, na.rm=T),
           y=max(Maize_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(MY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Maize Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Maize Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMY

Soy_this_piv <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year %in% experiment_year_range,]

Soy_this <- SoyYld_Mgha[SoyYld_Mgha$year %in% experiment_year_range,]
SYfit_time <- lm(APSIM ~ year, data = Soy_this)
SYfit_coef_time <- coef(SYfit_time)
SYfit_r2_time <- round(summary(SYfit_time)$r.squared,2)
SY_rmse_error_time <- Soy_this$Observed-Soy_this$APSIM
SY_rmse_time <- round(sqrt(mean(SY_rmse_error_time^2,na.rm=TRUE)),2)

# Soy_this <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year %in% experiment_year_range,]
# 
# SY_rmse_error <- pull(Soy_this[Soy_this$source=="Observed",],yield_val)-
#   pull(Soy_this[Soy_this$source=="APSIM",],"yield_val")
# SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)

gSY <- Soy_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Soy_this_piv$year, na.rm=T),
           y=max(Soy_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Soybean Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Soybean Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY

Wheat_this_piv <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year %in% experiment_year_range,]
Wheat_this <- WheatYld_Mgha[WheatYld_Mgha$year %in% experiment_year_range,]

WYfit_time <- lm(APSIM ~ year, data = Wheat_this)
WYfit_coef_time <- coef(WYfit_time)
WYfit_r2_time <- round(summary(WYfit_time)$r.squared,2)

WY_rmse_error_time <- Wheat_this$Observed-Wheat_this$APSIM
WY_rmse_time <- round(sqrt(mean(WY_rmse_error_time^2,na.rm=TRUE)),2)

# Wheat_this <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year %in% experiment_year_range,]
# 
# WY_rmse_error <- pull(Wheat_this[Wheat_this$source=="Observed",],yield_val)-
#   pull(Wheat_this[Wheat_this$source=="APSIM",],"yield_val")
# WY_rmse <- round(sqrt(mean(WY_rmse_error^2,na.rm=TRUE)),2)

gWY <- Wheat_this_piv%>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Wheat_this_piv$year, na.rm=T),
           y=max(Wheat_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(WY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Wheat Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Wheat Yield"),
          paste("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWY

# SOC

SOC_this_piv <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,]

## calculate stats with outliers
SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
Cfit_time <- lm(APSIM ~ year, data = SOC_this)
Cfit_coef_time <- coef(Cfit_time)
Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
C_rmse_error_time <- SOC_this$Observed-SOC_this$APSIM
C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)
## calculate stats without outliers
SOC_this_noout <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range &
                          !(Cstock_Mgha$Observed %in% ObsC_outliers),]
Cfit_time_noout <- lm(APSIM ~ year, data = SOC_this_noout)
Cfit_coef_time_noout <- coef(Cfit_time_noout)
Cfit_r2_time_noout <- round(summary(Cfit_time_noout)$r.squared,2)
C_rmse_error_time_noout <- SOC_this_noout$Observed-SOC_this_noout$APSIM
C_rmse_time_noout <- round(sqrt(mean(C_rmse_error_time_noout^2,na.rm=TRUE)),2)

## include outliers in trend line here
ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC <- SOC_this_piv %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  #ylim(10,35) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  #  geom_abline(intercept=Cfit_APSIM[1], slope=Cfit_APSIM[2], color="orange") +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 

# Soil temperature

Temp_this_piv <- SoilTemp_C_piv[SoilTemp_C_piv$year %in% experiment_year_range,]

Temp_this <- SoilTemp_C[year(SoilTemp_C$date) %in% experiment_year_range,]
Tfit_time <- lm(APSIM ~ date, data = Temp_this)
Tfit_coef_time <- coef(Tfit_time)
Tfit_r2_time <- round(summary(Tfit_time)$r.squared,2)
T_rmse_error_time <- Temp_this$Observed-Temp_this$APSIM
T_rmse_time <- round(sqrt(mean(T_rmse_error_time^2,na.rm=TRUE)),2)

gT <- Temp_this_piv[Temp_this_piv$source=='APSIM'
                     & Temp_this_piv$year %in% ObsTemp$year,] %>%
  ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=Temp_this_piv[Temp_this_piv$source=='Observed'
                                 & Temp_this_piv$year %in% ObsTemp$year,],
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

Moist_this_piv <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$year %in% experiment_year_range,]
Moist_this <- SoilMoist_VSM[year(SoilMoist_VSM$date) %in% experiment_year_range,]

Mfit_time <- lm(APSIM ~ date, data = Moist_this)
Mfit_coef_time <- coef(Mfit_time)
Mfit_r2_time <- round(summary(Mfit_time)$r.squared,2)

M_rmse_error_time <- Moist_this$Observed-Moist_this$APSIM
M_rmse_time <- round(sqrt(mean(M_rmse_error_time^2,na.rm=TRUE)),2)

gM <- Moist_this_piv[Moist_this_piv$source=='APSIM' 
                        & Moist_this_piv$year %in% ObsVSM$year,] %>%
  ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=Moist_this_piv[Moist_this_piv$source=='Observed' 
                                    & Moist_this_piv$year %in% ObsVSM$year,],
             aes(x=date, y=h2o_val, color=source)) +
  xlab("Year") +
  ylab("Volumetric soil moisture") +
  ggtitle(paste0(site_name," Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM

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

N2O_this_piv <- N2O_ghaday_piv[year(N2O_ghaday_piv$date) %in% experiment_year_range,]
N2O_this <- N2O_ghaday[year(N2O_ghaday$date) %in% experiment_year_range,]

Nfit_time <- lm(APSIM ~ date, data = N2O_this)
Nfit_coef_time <- coef(Nfit_time)
Nfit_r2_time <- round(summary(Nfit_time)$r.squared,2)

N_rmse_error_time <- N2O_this$Observed-N2O_this$APSIM
N_rmse_time <- round(sqrt(mean(N_rmse_error_time^2,na.rm=TRUE)),2)

gNG <- N2O_this_piv[N2O_this_piv$source=='APSIM' &
                        year(N2O_this_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=N2O_this_piv[N2O_this_piv$source=='Observed'&
                                   year(N2O_this_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source),size=1) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$n_rate_kg_ha>10,],
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
  ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                     values=cbPalette9[c(8,1,7)]) +
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
                           Fert$n_rate_kg_ha>10,],
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

# gNG_20ghd <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
#                         year(N2O_ghaday_piv$date) %in% year(ObsGas$date),] %>%
#   ggplot(aes(x=date, y=n2o_val, color=source)) +
#   geom_line(show.legend=TRUE) +
#   geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
#                                    year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
#              aes(x=date, y=n2o_val, color=source)) +
#   geom_segment(data=Fert[Fert$treatment==treatment & 
#                            Fert$date %in% year(ObsGas$date) &
#                            Fert$n_rate_kg_ha>10,],
#                aes(x = date, y = 200,
#                    xend = date, yend = 175),
#                colour=cbPalette9[7],
#                show.legend=F,
#                lineend = "round",
#                linejoin = "round",
#                arrow = arrow(length = unit(0.3, "cm"))
#                # colour = "black" 
#   ) + 
#   xlab("Year") +
#   ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
#   ylim(0,20) +
#   ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions-limited display to 20 g/ha/day"),
#           paste0("Scenario: ",scenario_descriptor)) +
#   scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
#                      values=cbPalette9[c(8,1,7)]) +
#   theme_classic(base_family = "serif", base_size = 15) +
#   theme(panel.background = element_blank(),
#         axis.line = element_line(),
#         legend.position = "right",
#         legend.key = element_blank())
# 
# gNG_20ghd


ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gMY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gSY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gWY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gC,
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
# ggsave(filename=paste0(results_path,"calib_N2O_comparison_20ghd_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_20ghd,
#        width=9, height=6, dpi=300)


#**********************************************************************

# 1:1 graphs --------------------------------------------------------------


MYfit <- lm(APSIM ~ Observed, data = MaizeYld_Mgha)
MYfit_coef <- coef(MYfit)
MYfit_r2 <- round(summary(MYfit)$r.squared,2)

MY_rmse_error <- MaizeYld_Mgha$Observed-MaizeYld_Mgha$APSIM
MY_rmse <- round(sqrt(mean(MY_rmse_error^2,na.rm=TRUE)),2)

gMY_121 <- MaizeYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=MYfit_coef[1], slope=MYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(MaizeYld_Mgha$Observed, MaizeYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha$Observed, MaizeYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(MYfit_coef[2],4))~"x" ~+ ~.(round(MYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(MaizeYld_Mgha$Observed, MaizeYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha$Observed, MaizeYld_Mgha$APSIM, na.rm=T)*.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(MYfit_r2))) +
  annotate("text", # RMSE
           x=min(MaizeYld_Mgha$Observed, MaizeYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha$Observed, MaizeYld_Mgha$APSIM, na.rm=T)*0.88,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(MY_rmse))) +
  ggtitle(bquote(.(site_name)~"Maize Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gMY_121

##
SYfit <- lm(APSIM ~ Observed, data = SoyYld_Mgha)
SYfit_coef <- coef(SYfit)
SYfit_r2 <- round(summary(SYfit)$r.squared,2)

SY_rmse_error <- SoyYld_Mgha$Observed-SoyYld_Mgha$APSIM
SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)

gSY_121 <- SoyYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoyYld_Mgha$Observed, SoyYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SoyYld_Mgha$Observed, SoyYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(SYfit_coef[2],4))~"x" ~+ ~.(round(SYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoyYld_Mgha$Observed, SoyYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SoyYld_Mgha$Observed, SoyYld_Mgha$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(SYfit_r2))) +
  annotate("text", # RMSE
           x=min(SoyYld_Mgha$Observed, SoyYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SoyYld_Mgha$Observed, SoyYld_Mgha$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse))) +
  ggtitle(bquote(.(site_name)~"Soybean Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gSY_121

##
WYfit <- lm(APSIM ~ Observed, data = WheatYld_Mgha)
WYfit_coef <- coef(WYfit)
WYfit_r2 <- round(summary(WYfit)$r.squared,2)

WY_rmse_error <- WheatYld_Mgha$Observed-WheatYld_Mgha$APSIM
WY_rmse <- round(sqrt(mean(WY_rmse_error^2,na.rm=TRUE)),2)

gWY_121 <- WheatYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=WYfit_coef[1], slope=WYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(WheatYld_Mgha$Observed, WheatYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(WheatYld_Mgha$Observed, WheatYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(WYfit_coef[2],4))~"x" ~+ ~.(round(WYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(WheatYld_Mgha$Observed, WheatYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(WheatYld_Mgha$Observed, WheatYld_Mgha$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(WYfit_r2))) +
  annotate("text", # RMSE
           x=min(WheatYld_Mgha$Observed, WheatYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(WheatYld_Mgha$Observed, WheatYld_Mgha$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(WY_rmse))) +
  ggtitle(bquote(.(site_name)~"Wheat Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gWY_121

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

##
Mfit <- lm(APSIM ~ Observed, data = SoilMoist_VSM)
Mfit_coef <- coef(Mfit)
Mfit_r2 <- round(summary(Mfit)$r.squared,2)

M_rmse_error <- SoilMoist_VSM$Observed-SoilMoist_VSM$APSIM
M_rmse <- round(sqrt(mean(M_rmse_error^2,na.rm=TRUE)),2)

gM_121 <- SoilMoist_VSM %>%
  ggplot(aes(x=Observed, y=APSIM, 
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T), 
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Mfit_coef[1], slope=Mfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1.1,
           y=max(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Mfit_coef[2],4))~"x" ~+ ~.(round(Mfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1.1,
           y=max(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Mfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*1.1,
           y=max(SoilMoist_VSM$Observed, SoilMoist_VSM$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(M_rmse))) +
  ggtitle(paste0(site_name," Volumetric Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gM_121

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

##
Nfit <- lm(APSIM ~ Observed, data = N2O_ghaday)
Nfit_coef <- coef(Nfit)
Nfit_r2 <- round(summary(Nfit)$r.squared,2)

N_rmse_error <- N2O_ghaday$Observed-N2O_ghaday$APSIM
N_rmse <- round(sqrt(mean(N_rmse_error^2,na.rm=TRUE)),2)

gNG_121 <- N2O_ghaday[!is.na(N2O_ghaday$Observed),] %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  xlim(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T), 
       max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)) +
  ylim(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T), 
       max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Nfit_coef[1], slope=Nfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=abs(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1.1),
           y=max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Nfit_coef[2],4))~"x" ~+ ~.(round(Nfit_coef[1],4)))) +
  annotate("text", # R^2
           x=abs(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1.1),
           y=max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Nfit_r2))) +
  annotate("text", # RMSE
           x=abs(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*1.1),
           y=max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(N_rmse))) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gNG_121

ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gMY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gSY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gWY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gT_121, width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_calib_",scenario_name,"_APSIM.jpg"),plot=gT_121_calib)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gM_121, width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_calib_",scenario_name,"_APSIM.jpg"),plot=gM_121_calib)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gNG_121, width=6, height=6, dpi=300)


#**********************************************************************

# Log results -------------------------------------------------------------


# add this run's results to model log file and file collecting all final
# model runs

calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                       clim_scenario_num,mgmt_scenario_num, scenario_name,
                       scenario_abbrev,
                       MYfit_coef[2], MYfit_coef[1], MYfit_r2, MY_rmse,
                       Maize_obsmod_diff_Mgha,
                       SYfit_coef[2], SYfit_coef[1], SYfit_r2, SY_rmse,
                       Soybean_obsmod_diff_Mgha,
                       WYfit_coef[2], WYfit_coef[1], WYfit_r2, WY_rmse,
                       Wheat_obsmod_diff_Mgha,
                       Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse,
                       SOC_obsmod_diff_Mgha,SOC_obsmod_diff_Mgha_nooutliers,
                       Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse,
                       SoilT_obsmod_diff_Mgha,
                       Mfit_coef[2], Mfit_coef[1], Mfit_r2, M_rmse,
                       SoilM_obsmod_diff_Mgha,
                       Nfit_coef[2], Nfit_coef[1], Nfit_r2, N_rmse,
                       N2O_obsmod_diff_gha,
                       NA, NA, NA, NA, # CH4
                       NA,
                       NA, NA, NA, NA, # M Bio
                       NA,
                       NA, NA, NA, NA, # Cotton
                       NA,
                       NA, NA, NA, NA, # Sorghum
                       NA,
                       NA, NA, NA, NA, NA, # M,S,W,C,S cultivars
                       MYfit_coef_time[2], MYfit_coef_time[1], MYfit_r2_time, MY_rmse_time,
                       SYfit_coef_time[2], SYfit_coef_time[1], SYfit_r2_time, SY_rmse_time,
                       WYfit_coef_time[2], WYfit_coef_time[1], WYfit_r2_time, WY_rmse_time,
                       Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, C_rmse_time,
                       Cfit_coef_time_noout[2], Cfit_coef_time_noout[1], Cfit_r2_time_noout, C_rmse_time_noout,
                       Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, T_rmse_time,
                       Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, M_rmse_time,
                       Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, N_rmse_time,
                       NA, NA, NA, NA, # methane
                       NA, NA, NA, NA, # microbio
                       NA, NA, NA, NA, # cotton
                       NA, NA, NA, NA # sorghum
)

source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)

rm(calib_log_tab,
   MYfit_coef, MYfit_r2, MY_rmse,
   Maize_obsmod_diff_Mgha,
   SYfit_coef, SYfit_r2, SY_rmse,
   Soybean_obsmod_diff_Mgha,
   WYfit_coef, WYfit_r2, WY_rmse,
   Wheat_obsmod_diff_Mgha,
   Cfit_coef, Cfit_r2, C_rmse,
   SOC_obsmod_diff_Mgha,SOC_obsmod_diff_Mgha_nooutliers,
   Tfit_coef, Tfit_r2, T_rmse,
   SoilT_obsmod_diff_Mgha,
   Mfit_coef, Mfit_r2, M_rmse,
   SoilM_obsmod_diff_Mgha,
   Nfit_coef, Nfit_r2, N_rmse,
   N2O_obsmod_diff_gha,
   MYfit_coef_time, MYfit_r2_time, MY_rmse_time,
   SYfit_coef_time, SYfit_r2_time, SY_rmse_time,
   WYfit_coef_time, WYfit_r2_time, WY_rmse_time,
   Cfit_coef_time, Cfit_r2_time, C_rmse_time,
   Cfit_coef_time_noout, 
   Cfit_r2_time_noout, C_rmse_time_noout,
   Tfit_coef_time, Tfit_r2_time, T_rmse_time,
   Mfit_coef_time, Mfit_r2_time, M_rmse_time,
   Nfit_coef_time, Nfit_r2_time, N_rmse_time)


}) # end suppressMessages