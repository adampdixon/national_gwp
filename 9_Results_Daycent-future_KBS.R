# title: "9_Results_Daycent-future.R"
# author: "Ellen Maas"
# date: "8/8/2022"
# output: html_document
# ---

suppressMessages({
  
  print("Starting 9_Results_Daycent-future.R")
  
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  library(broom)
  
  # Future temporal graphs
  
  ## Soil Moisture
  
  gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Daycent'
                          &SoilMoist_VSM_piv$date>=experiment_start_date,] %>%
    ggplot(aes(x=date, y=h2o_val, color=source)) +
    geom_point() +
    geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed'
                                      &SoilMoist_VSM_piv$date>=experiment_start_date,],
               aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture (%)") +
    ggtitle(paste0(site_name," Volumetric soil moisture with ",soil_moist_bias,"% correction"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  ## Maize
  
  MYfit_Day <- coef(lm(Daycent ~ year, 
                       data = MaizeYld_Mgha[MaizeYld_Mgha$year>end_exp_period_year,]))
  MYfit_Obs <- coef(lm(Observed ~ year, 
                       data = MaizeYld_Mgha[MaizeYld_Mgha$year %in% experiment_year_range,]))
  MYxs <- c(end_exp_period_year+1, end_fut_period_year)
  MYys <- cbind(1, MYxs) %*% MYfit_Day
  MYobsxs <- c(experiment_start_year, experiment_end_year)
  MYobsys <- cbind(1, MYobsxs) %*% MYfit_Obs
  
  gMY <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year>=experiment_start_date,] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ggtitle(paste(site_name,"Maize Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    #    geom_abline(intercept=MYfit_Day[1], slope=MYfit_Day[2], color="orange") +
    geom_segment(aes(x = MYxs[1], xend = MYxs[2], y = MYys[1], yend = MYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = MYobsxs[1], xend = MYobsxs[2], y = MYobsys[1], yend = MYobsys[2]), color=cbPalette9[1]) +
    ylab(expression('Maize Yield (Mg ha ' ^-1*')')) +
    scale_color_manual(labels=c("Daycent","Historical","Observed"),
                       values=cbPalette9[c(8,4,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMY
  
  ## Soybeans
  
  SYfit_Day <- coef(lm(Daycent ~ year, 
                       data = SoyYld_Mgha[SoyYld_Mgha$year>end_exp_period_year,]))
  SYfit_Obs <- coef(lm(Observed ~ year, 
                       data = SoyYld_Mgha[SoyYld_Mgha$year %in% experiment_year_range,]))
  SYxs <- c(end_exp_period_year+1, end_fut_period_year)
  SYys <- cbind(1, SYxs) %*% SYfit_Day
  SYobsxs <- c(experiment_start_year, experiment_end_year)
  SYobsys <- cbind(1, SYobsxs) %*% SYfit_Obs
  
  gSY <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year>=experiment_start_date,] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Soybean Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = SYxs[1], xend = SYxs[2], y = SYys[1], yend = SYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = SYobsxs[1], xend = SYobsxs[2], y = SYobsys[1], yend = SYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("Daycent","Historical","Observed"),
                       values=cbPalette9[c(8,4,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSY
  
  ## Wheat
  
  WYfit_Day <- coef(lm(Daycent ~ year, 
                       data = WheatYld_Mgha[WheatYld_Mgha$year>end_exp_period_year+1,]))
  WYfit_Obs <- coef(lm(Observed ~ year, 
                       data = WheatYld_Mgha[WheatYld_Mgha$year %in% experiment_year_range,]))
  WYxs <- c(end_exp_period_year+1, end_fut_period_year)
  WYys <- cbind(1, WYxs) %*% WYfit_Day
  WYobsxs <- c(experiment_start_year, experiment_end_year)
  WYobWYs <- cbind(1, WYobsxs) %*% WYfit_Obs
  
  gWY <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year>=experiment_start_date,] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Wheat Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = WYxs[1], xend = WYxs[2], y = WYys[1], yend = WYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = WYobsxs[1], xend = WYobsxs[2], y = WYobWYs[1], yend = WYobWYs[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("Daycent","Historical","Observed"),
                       values=cbPalette9[c(8,4,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gWY
  
  ## SOC
  
  Cfit_Day <- coef(lm(Daycent ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= end_exp_period_year+1,]))
  if(mgmt_scenario_grp==3) {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                              Cstock_Mgha$year >= experiment_start_year,]))
  } else {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  }
  
  Cxs <- c(end_exp_period_year+1, end_fut_period_year)
  Cys <- cbind(1, Cxs) %*% Cfit_Day
  Cobsxs <- c(experiment_start_year, experiment_end_year)
  Cobsys <- cbind(1, Cobsxs) %*% Cfit_Obs
  
  gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year>=experiment_start_date,] %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
    ggtitle(paste(site_name,"Soil Organic Carbon"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = Cxs[1], xend = Cxs[2], y = Cys[1], yend = Cys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = Cobsxs[1], xend = Cobsxs[2], y = Cobsys[1], yend = Cobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  
  # #first(SoilTemp_C_piv[SoilTemp_C_piv$source=="Observed"&!is.na(SoilTemp_C_piv$temp_val),"date"])
  # 
  # m_Tfit_Daycent_pre2010 <- lm(temp_val ~ date, 
  #                              data = SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
  #                                                      SoilTemp_C_piv$date>="1999-01-01"&
  #                                                      SoilTemp_C_piv$source=="Daycent",])
  # m_Tfit_Obs_pre2010 <- lm(temp_val ~ date, 
  #                          data = SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
  #                                                  SoilTemp_C_piv$source=="Observed",])
  # Tfit_Daycent_aug_pre2010 <- augment(m_Tfit_Daycent_pre2010, 
  #                                     SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
  #                                                      SoilTemp_C_piv$date>="1999-01-01"&
  #                                                      SoilTemp_C_piv$source=="Daycent",])
  # Tfit_Obs_aug_pre2010 <- augment(m_Tfit_Obs_pre2010, 
  #                                 SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
  #                                                  SoilTemp_C_piv$source=="Observed"&
  #                                                  !is.na(SoilTemp_C_piv$temp_val),])
  # m_Tfit_Daycent <- lm(temp_val ~ date, data = SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
  #                                                              SoilTemp_C_piv$source=="Daycent",])
  # m_Tfit_Obs <- lm(temp_val ~ date, data = SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
  #                                                              SoilTemp_C_piv$source=="Observed",])
  # Tfit_Daycent_aug <- augment(m_Tfit_Daycent, SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
  #                                                              SoilTemp_C_piv$source=="Daycent",])
  # Tfit_Obs_aug <- augment(m_Tfit_Obs, SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
  #                                                  SoilTemp_C_piv$source=="Observed"&
  #                                                  !is.na(SoilTemp_C_piv$temp_val),])
  
  gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='Daycent'
                       &SoilTemp_C_piv$date>=experiment_start_date,] %>%
    ggplot(aes(x=date, y=temp_val, color=source)) +
    geom_point(show.legend=TRUE) +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed'
                                   &SoilTemp_C_piv$date>=experiment_start_date,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    # geom_line(data=Tfit_Daycent_aug_pre2010, aes(y=.fitted),show.legend=F) + 
    # geom_line(data=Tfit_Obs_aug_pre2010, aes(y=.fitted),show.legend=F) + 
    # geom_line(data=Tfit_Daycent_aug, aes(y=.fitted),show.legend=F) + 
    # geom_line(data=Tfit_Obs_aug, aes(y=.fitted),show.legend=F) + 
    #geom_abline(intercept=Tfit_Daycent[1], slope=Tfit_Daycent[2], color="orange") +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  gTfull <- SoilTemp_C_piv[SoilTemp_C_piv$source=='Daycent',] %>%
    ggplot(aes(x=date, y=temp_val, color=source)) +
    geom_point(show.legend=TRUE) +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed'
                                   &SoilTemp_C_piv$date>=experiment_start_date,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    # geom_line(data=Tfit_Daycent_aug_pre2010, aes(y=.fitted),show.legend=F) + 
    # geom_line(data=Tfit_Obs_aug_pre2010, aes(y=.fitted),show.legend=F) + 
    # geom_line(data=Tfit_Daycent_aug, aes(y=.fitted),show.legend=F) + 
    # geom_line(data=Tfit_Obs_aug, aes(y=.fitted),show.legend=F) + 
    #geom_abline(intercept=Tfit_Daycent[1], slope=Tfit_Daycent[2], color="orange") +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  
  # gB <- SoilBD_gcc_piv %>%
  #   ggplot(aes(x=source,y=bd_val, fill=factor(year))) +
  #   geom_col(position="dodge") +
  #   ylab(expression('Bulk density (g cc' ^-1*')')) +
  #   labs(fill="Year") +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line())
  # 
  # gB
  
  # Daily N2O
  
  gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='Daycent'
                        &year(N2O_ghaday_piv$date) >= experiment_start_date,] %>%
    ggplot(aes(x=date, y=n2o_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'
                                   &year(N2O_ghaday_piv$date) >= experiment_start_date,],
               aes(x=date, y=n2o_val, color=source)) +
    # geom_segment(data=Fert[Fert$treatment=="T1"&Fert$n_rate_kg_ha>10
    #                        &year(Fert$date) >= experiment_start_date,],
    #              aes(x = date, y = 200,
    #                  xend = date, yend = 175),
    #                  colour=cbPalette9[7],
    #                  show.legend=F,
    #                  lineend = "round",
    #                  linejoin = "round",
    #                  arrow = arrow(length = unit(0.3, "cm"))
    # colour = "black" 
    #                 ) + 
  xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g N ha ' ^-1*'day ' ^-1*')')) +
    # scale_color_manual(labels=c("Daycent","Observed","Fertilizer"),
    #                    values=cbPalette9[c(8,1,7)]) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNG
  
  # Annual N2O
  
  NGfit_Day <- coef(lm(N2OEmissions_ghayr ~ year, 
                       data = DayGN_ann_gha[DayGN_ann_gha$year>end_exp_period_year,]))
  NGxs <- c(end_exp_period_year+1, end_fut_period_year)
  NGys <- cbind(1, NGxs) %*% NGfit_Day
  
  gNGann <- N2O_ghayr_piv %>%
    ggplot(aes(x=year, y=n2o_val, color=source)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Annual Cumulative N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = NGxs[1], xend = NGxs[2], y = NGys[1], yend = NGys[2]), color=cbPalette9[8]) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(8)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNGann
  
  ## Daily CH4
  
  #first(CH4_ghaday_piv[CH4_ghaday_piv$source=="Observed"&!is.na(CH4_ghaday_piv$ch4_val),"date"])
  #last(CH4_ghaday_piv[CH4_ghaday_piv$source=="Observed"&!is.na(CH4_ghaday_piv$ch4_val),"date"])
  
  gMG <- CH4_ghaday_piv[CH4_ghaday_piv$source=='Daycent'
                        &year(CH4_ghaday_piv$date) >= experiment_start_date,] %>%
    ggplot(aes(x=date, y=ch4_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=CH4_ghaday_piv[CH4_ghaday_piv$source=='Observed'
                                   &year(CH4_ghaday_piv$date) >= experiment_start_date,],
               aes(x=date, y=ch4_val, color=source)) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (g C ha ' ^-1*'day ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Total Cumulative CH"["4"]*" Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMG
  
  ## Annual CH4
  
  MGfit_Day <- coef(lm(CH4Emissions_ghayr ~ year, 
                       data = DayGM_ann_gha[DayGM_ann_gha$year>end_exp_period_year+1,]))
  MGxs <- c(end_exp_period_year+1, end_fut_period_year)
  MGys <- cbind(1, MGxs) %*% MGfit_Day
  
  gMGann <- CH4_ghayr_piv %>%
    ggplot(aes(x=year, y=ch4_val, color=source)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Annual Cumulative CH"["4"]*" Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = MGxs[1], xend = MGxs[2], y = MGys[1], yend = MGys[2]), color=cbPalette9[8]) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(8)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMGann
  
  ## Carbon Input
  
  gCI <- DayCI_gm2yr %>%
    ggplot(aes(x=year, y=base), show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('C input (g C m' ^-2*' yr' ^-1*')')) +
    ggtitle(paste(site_name,"Soil C Input"),
            paste0("Scenario: ",scenario_descriptor)) +
    # scale_color_manual(labels=c("Daycent","Observed"),
    #                    values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCI
  
  gNI <- DayNI_gm2yr[DayNI_gm2yr$year >= experiment_start_date,] %>%
    ggplot(aes(x=year, y=base), show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N input (g C m' ^-2*' yr' ^-1*')')) +
    ylim(0,12) +
    ggtitle(paste(site_name,"Soil N Input"),
            paste0("Scenario: ",scenario_descriptor)) +
    # scale_color_manual(labels=c("Daycent","Observed"),
    #                    values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNI
  
  gNH4 <- Day_soiln_all[Day_soiln_all$year >= experiment_start_date,] %>%
    ggplot(aes(x=year, y=ammonium)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('NH4 ppm per day')) +  
    ylim(0,125) +
    ggtitle(bquote(.(site_name)~"Soil NH"["4"]*" - top 10 cm"),
            paste0("Scenario: ",scenario_descriptor)) +
    # scale_color_manual(labels=c("Daycent","Observed"),
    #                    values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNH4
  
  gNO3 <- Day_soiln_all[Day_soiln_all$year >= experiment_start_date,] %>%
    ggplot(aes(x=year, y=NO3_ppm)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('NO3 ppm per day')) +  
    ylim(0,125) +
    ggtitle(bquote(.(site_name)~"Soil NO"["3"]*" - top 20 cm"),
            paste0("Scenario: ",scenario_descriptor)) +
    # scale_color_manual(labels=c("Daycent","Observed"),
    #                    values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNO3
  
  # explain N2O emissions with WFPS, NO3 and N2O
  # transform_factor <- 200
  # 
  # gN2O_expl <- ggplot() +
  #   geom_hline(aes(yintercept=0.26, color=cbPalette9[6]), linewidth=1) + # DUL, from APSIM surface layer 0-20 cm
  #   geom_line(data=Day_soiln[Day_soiln$year %in% 2010:2015,],
  #             aes(x=date,y=NO3_kgha/transform_factor, color=cbPalette9[4]), linewidth=1) +
  #   geom_line(data=N2O_ghaday[year(N2O_ghaday$date) %in% 2010:2015,],
  #             aes(x=date, y=Daycent/1000, color=cbPalette9[8]), linewidth=1) +
  #   geom_line(data=SoilMoist_VSM[SoilMoist_VSM$year %in% 2010:2015,],
  #             aes(x=date, y=Daycent, color=cbPalette9[2]), linewidth=1) +
  #   ylab(expression('Soil Water, Field Capacity, N'[2]*'O  (kg ha' ^'-1'*' day'^'-1'*')')) +
  #   scale_y_continuous(
  #     sec.axis = sec_axis(trans = ~ .x * transform_factor,
  #                         name = expression('NO'[3]*' (kg ha' ^'-1'*' day'^'-1'*')'))
  #   ) +
  #   scale_color_manual(name=NULL,
  #                      labels=c("Soil Water","NO3 (kg/ha)","Field Capacity","N2O (kg/ha)"),
  #                      values=cbPalette9[c(2,4,6,8)]) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # 
  # gN2O_expl
  
  gN2O_expl_0to10cm <- ggplot() +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer1, color=cbPalette9[2]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer2, color=cbPalette9[3]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer3, color=cbPalette9[4]), linewidth=1) +
    geom_line(data=Day_exp_soiln[Day_exp_soiln$year %in% 2010:2011,],
              aes(x=date,y=NO3_hgha/10, color=cbPalette9[6]), linewidth=1) +
    geom_line(data=N2O_ghaday[year(N2O_ghaday$date) %in% 2010:2011,],
              aes(x=date, y=Daycent/100, color=cbPalette9[8]), linewidth=1) +
    ggtitle("Daycent N2O emissions drivers to 10 cm") +
    ylab(expression('WFPS, NO'[3]*' (dg ha' ^'-1'*' day, N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
    scale_color_manual(name=NULL,
                       labels=c("WFPS: 0-2.5cm","WFPS: 2.5-5 cm","WFPS: 5-10 cm",
                                "NO3 (dg/ha/day)","N2O (cg/ha/day)"),
                       values=cbPalette9[c(2,3,4,6,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gN2O_expl_0to10cm
  
  gN2O_expl_10to60cm <- ggplot() +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer4, color=cbPalette9[2]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer5, color=cbPalette9[3]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer6, color=cbPalette9[4]), linewidth=1) +
    geom_line(data=Day_exp_soiln[Day_exp_soiln$year %in% 2010:2011,],
              aes(x=date,y=NO3_hgha_10to60cm/10, color=cbPalette9[6]), linewidth=1) +
    geom_line(data=N2O_ghaday[year(N2O_ghaday$date) %in% 2010:2011,],
              aes(x=date, y=Daycent/100, color=cbPalette9[8]), linewidth=1) +
    ggtitle("Daycent N2O emissions drivers 10 to 60 cm") +
    ylab(expression('WFPS, NO'[3]*' (dg ha' ^'-1'*' day, N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
    scale_color_manual(name=NULL,
                       labels=c("WFPS: 10-20 cm","WFPS: 20-40 cm","WFPS: 40-60 cm",
                                "NO3 (dg/ha/day)","N2O (cg/ha/day)"),
                       values=cbPalette9[c(2,3,4,6,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gN2O_expl_10to60cm
  
  # explain CH4 emissions with 
  
  gCH4_expl_prod_0to10cm <- ggplot() +
    # geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
    #           aes(x=date, y=wfps_layer1, color=cbPalette9[2]), linewidth=1) +
    # geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
    #           aes(x=date, y=wfps_layer2, color=cbPalette9[3]), linewidth=1) +
    # geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
    #           aes(x=date, y=wfps_layer3, color=cbPalette9[4]), linewidth=1) +
    # geom_line(data=Day_exp_methane[year(Day_exp_methane$date) %in% 2010:2011,],
    #           aes(x=date, y=Feh*100, color=cbPalette9[5]), linewidth=1) +
    geom_line(data=Day_exp_methane[year(Day_exp_methane$date) %in% 2010:2011,],
              aes(x=date, y=COM, color=cbPalette9[6]), linewidth=1) +
    geom_line(data=Day_exp_methane[year(Day_exp_methane$date) %in% 2010:2011,],
              aes(x=date, y=TI, color=cbPalette9[7]), linewidth=1) +
    geom_line(data=Day_exp_methane[year(Day_exp_methane$date) %in% 2010:2011,],
              aes(x=date, y=Cr, color=cbPalette9[1]), linewidth=1) +
    geom_line(data=Day_exp_methane[Day_exp_methane$year %in% 2010:2011,],
              aes(x=date,y=CH4_prod*1000, color=cbPalette9[8]), linewidth=1) +
    geom_line(data=Day_exp_methane[Day_exp_methane$year %in% 2010:2011,],
              aes(x=date,y=CH4_oxid*-1000, color=cbPalette9[4]), linewidth=1) +
    ggtitle("Daycent CH4 production drivers") +
    ylab('') +
    scale_color_manual(name=NULL,
                       # labels=c("WFPS: 0-2.5cm","WFPS: 2.5-5 cm","WFPS: 5-10 cm",
                       #          "Eh/100","COM","Cr","CH4 (mg/m^2/day)"),
                       labels=c("Root Exudate C (index)","CH4 oxid (mg/m^2/day)",
                                "OM Decomp C (index)",
                                "Soil Temp (index)","CH4 prod (mg/m^2/day)"),
                       values=cbPalette9[c(1,4,6,7,8)]) + #,6,7,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gCH4_expl_prod_0to10cm

  # CH4 oxidation explained by soil temperature, wfps, vswc in top 15 cm
  ch4_transform_factor <- 100
  
  gCH4_expl_oxid_0to20cm <- ggplot() +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer1, color=cbPalette9[2]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer2, color=cbPalette9[3]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer3, color=cbPalette9[4]), linewidth=1) +
    geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
              aes(x=date, y=wfps_layer4, color=cbPalette9[5]), linewidth=1) +
    geom_line(data=Day_exp_soiltavg[Day_exp_soiltavg$year %in% 2010:2011,],
              aes(x=date, y=layer1/100, color=cbPalette9[7]), linewidth=1) +
    geom_line(data=Day_exp_methane[Day_exp_methane$year %in% 2010:2011,],
              aes(x=date,y=CH4_oxid*-1000, color=cbPalette9[8]), linewidth=1) +
    ggtitle("Daycent CH4 oxidation drivers") +
    ylab(expression('WFPS, CH'[4]*'  (mg m' ^'-2'*' day'^'-1'*')')) +
      scale_y_continuous(
        sec.axis = sec_axis(trans = ~ .x * ch4_transform_factor,
                            name = expression('Soil Temperature ('^o*'C)'))
      ) +
    scale_color_manual(name=NULL,
                       labels=c("WFPS: 0-2.5cm","WFPS: 2.5-5 cm","WFPS: 5-10 cm",
                                "WFPS: 10-20cm","Soil Temp","CH4 (mg/m^2/day"),
                       values=cbPalette9[c(2,3,4,5,7,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gCH4_expl_oxid_0to20cm
  

  
  ggsave(filename=paste0(results_path,"Maize_yield_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gMY,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soybean_yield_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gSY,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Wheat_yield_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gWY,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"SOC_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gC,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Temp_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gT,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Moist_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gM,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gNG,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_ann_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gNGann, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"CH4_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gMG,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"CH4_ann_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gMGann,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"C_input_fut_",scenario_name,"_Daycent.jpg"),plot=gCI,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"NH4_input_fut_",scenario_name,"_Daycent.jpg"),plot=gNH4,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"NO3_input_fut_",scenario_name,"_Daycent.jpg"),plot=gNO3,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_N2O_0to10cm",scenario_name,"_Daycent.jpg"),plot=gN2O_expl_0to10cm,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_N2O_10to60cm",scenario_name,"_Daycent.jpg"),plot=gN2O_expl_10to60cm,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_CH4_prod_0to10cm",scenario_name,"_Daycent.jpg"),plot=gCH4_expl_prod_0to10cm,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_CH4_oxid_0to20cm",scenario_name,"_Daycent.jpg"),plot=gCH4_expl_oxid_0to20cm,
         width=9, height=6, dpi=300)
  
  
}) # end suppressMessages