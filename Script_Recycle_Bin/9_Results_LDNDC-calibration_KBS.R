#title: 9_Results_LDNDC-calibration_KBS.R
#author: Ellen Maas
#date: 7/22/2022
#output: html_document
#description: 

suppressMessages({
  
  print(paste0("Starting 9_Results_LDNDC-calibration_",site_name,".R"))
  
  library(magrittr)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  
  #**********************************************************************
  
  # Temporal graphs ---------------------------------------------------------
  
  
  # suggested to calibrate in this order:
  # - soil water content
  # - crop yields and plant growth rates
  # - soil organic C
  # - N loss
  
  #first(SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=="KBS_Observed"&!is.na(SoilMoist_VSM_piv$h2o_val),"date"])
  
  gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='LDNDC'
                          & SoilMoist_VSM_piv$date>="2003-01-01"
                          & SoilMoist_VSM_piv$date<experiment_end_date,] %>%
    ggplot(aes(x=date, y=h2o_val, color=source)) +
    geom_point() +
    geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed'
                                      & SoilMoist_VSM_piv$date>="2003-01-01"
                                      & SoilMoist_VSM_piv$date<experiment_end_date,],
               aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture (%)") +
    ggtitle(paste0(site_name," Soil Moisture"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("LDNDC","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  
  Maize_this <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year %in% experiment_year_range &
                                    MaizeYld_Mgha_piv$source!='Historical',]
  
  MY_rmse_error <- pull(Maize_this[Maize_this$source=="Observed",],yield_val)-
    pull(Maize_this[Maize_this$source=="LDNDC",],"yield_val")
  MY_rmse <- round(sqrt(mean(MY_rmse_error^2,na.rm=TRUE)),2)
  
  gMY <- Maize_this %>%
    ggplot(aes(x=year, y=yield_val, color=source)) +
    geom_point() +
    annotate("text", # RMSE
             x=min(Maize_this$year, na.rm=T),
             y=max(Maize_this$yield_val, na.rm=T),
             hjust=0, family="serif", color="gray31",
             label=bquote("RMSE =" ~.(MY_rmse))) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Maize Yield"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("LDNDC","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMY
  
  gMhY <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year <= experiment_end_year,] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ggtitle(paste(site_name,"Maize Yield"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("Historical","LDNDC","Observed"),
                       values=cbPalette9[c(4,8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMhY
  

  Soy_this <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year %in% experiment_year_range &
                                SoyYld_Mgha_piv$source!='Historical',]
  
  SY_rmse_error <- pull(Soy_this[Soy_this$source=="Observed",],yield_val)-
    pull(Soy_this[Soy_this$source=="LDNDC",],"yield_val")
  SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)
  
  gSY <- Soy_this %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    annotate("text", # RMSE
             x=min(Soy_this$year, na.rm=T),
             y=max(Soy_this$yield_val, na.rm=T),
             hjust=0, family="serif", color="gray31",
             label=bquote("RMSE =" ~.(SY_rmse))) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Soybean Yield"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("LDNDC","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSY
  
  gShY <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year <= experiment_end_year,] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Soybean Yield"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("Historical","LDNDC","Observed"),
                       values=cbPalette9[c(4,8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gShY
  
  
  Wheat_this <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year %in% experiment_year_range &
                                    WheatYld_Mgha_piv$source!='Historical',]
  
  WY_rmse_error <- pull(Wheat_this[Wheat_this$source=="Observed",],yield_val)-
    pull(Wheat_this[Wheat_this$source=="LDNDC",],"yield_val")
  WY_rmse <- round(sqrt(mean(WY_rmse_error^2,na.rm=TRUE)),2)
  
  gWY <- Wheat_this %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    annotate("text", # RMSE
             x=min(Wheat_this$year, na.rm=T),
             y=max(Wheat_this$yield_val, na.rm=T),
             hjust=0, family="serif", color="gray31",
             label=bquote("RMSE =" ~.(WY_rmse))) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Wheat Yield"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("LDNDC","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gWY
  
  gWhY <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year <= experiment_end_year,] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Wheat Yield"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("Historical","LDNDC","Observed"),
                       values=cbPalette9[c(4,8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gWhY
  
  
  ##
  if(mgmt_scenario_grp==3) {
    Cfit_LDNDC <- coef(lm(LDNDC ~ year, data = Cstock_Mgha))
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                              Cstock_Mgha$year >= experiment_start_year,]))
  } else {
    Cfit_LDNDC <- coef(lm(LDNDC ~ year, data = Cstock_Mgha))
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  }
  
  gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,] %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
    #ylim(10,35) +
    ggtitle(paste(site_name,"Soil Organic Carbon"),
            paste0("Scenario: ",scenario_descriptor)) +
    #  geom_abline(intercept=Cfit_LDNDC[1], slope=Cfit_LDNDC[2], color="orange") +
    geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
    scale_color_manual(labels=c("LDNDC","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  

  #first(SoilTemp_C_piv[SoilTemp_C_piv$source=="KBS_Observed"&!is.na(SoilTemp_C_piv$temp_val),"date"])
  
  m_Tfit_LDNDC_pre2010 <- lm(temp_val ~ date, 
                               data = SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
                                                       SoilTemp_C_piv$date>="1999-01-01"&
                                                       SoilTemp_C_piv$source=="LDNDC",])
  m_Tfit_Obs_pre2010 <- lm(temp_val ~ date, 
                           data = SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
                                                   SoilTemp_C_piv$source=="Observed",])
  Tfit_LDNDC_aug_pre2010 <- augment(m_Tfit_LDNDC_pre2010, 
                                      SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
                                                       SoilTemp_C_piv$date>="1999-01-01"&
                                                       SoilTemp_C_piv$source=="LDNDC",])
  Tfit_Obs_aug_pre2010 <- augment(m_Tfit_Obs_pre2010, 
                                  SoilTemp_C_piv[SoilTemp_C_piv$date<="2010-01-01"&
                                                   SoilTemp_C_piv$source=="Observed"&
                                                   !is.na(SoilTemp_C_piv$temp_val),])
  m_Tfit_LDNDC <- lm(temp_val ~ date, data = SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
                                                                SoilTemp_C_piv$source=="LDNDC",])
  m_Tfit_Obs <- lm(temp_val ~ date, data = SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
                                                            SoilTemp_C_piv$source=="Observed",])
  Tfit_LDNDC_aug <- augment(m_Tfit_LDNDC, SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
                                                               SoilTemp_C_piv$source=="LDNDC",])
  Tfit_Obs_aug <- augment(m_Tfit_Obs, SoilTemp_C_piv[SoilTemp_C_piv$date>="2010-01-01"&
                                                       SoilTemp_C_piv$source=="Observed"&
                                                       !is.na(SoilTemp_C_piv$temp_val),])
  
  gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='LDNDC'
                       & SoilTemp_C_piv$date>="1999-01-01"
                       & SoilTemp_C_piv$date<experiment_end_date,] %>%
    ggplot(aes(x=date, y=temp_val, color=source)) +
    geom_point(show.legend=TRUE) +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed'
                                   & SoilTemp_C_piv$date>="1999-01-01"
                                   & SoilTemp_C_piv$date<experiment_end_date,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    ggtitle(paste0(site_name," Soil Temperature"),
            paste0("Scenario: ",scenario_descriptor)) +
    #geom_line(data=Tfit_LDNDC_aug_pre2010, aes(y=.fitted),show.legend=F) + 
    #geom_line(data=Tfit_Obs_aug_pre2010, aes(y=.fitted),show.legend=F) + 
    #geom_line(data=Tfit_LDNDC_aug, aes(y=.fitted),show.legend=F) + 
    #geom_line(data=Tfit_Obs_aug, aes(y=.fitted),show.legend=F) + 
    #geom_abline(intercept=Tfit_LDNDC[1], slope=Tfit_LDNDC[2], color="orange") +
    scale_color_manual(labels=c("LDNDC","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  mean_soilT_obs <- mean(SoilTemp_C[,"Observed"],na.rm=TRUE)
  mean_soilT_day <- mean(SoilTemp_C[,"LDNDC"],na.rm=TRUE)
  
  
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
  
  #first(N2O_ghaday_piv[N2O_ghaday_piv$source=="KBS_Observed"&!is.na(N2O_ghaday_piv$n2o_val),"date"])
  #last(N2O_ghaday_piv[N2O_ghaday_piv$source=="KBS_Observed"&!is.na(N2O_ghaday_piv$n2o_val),"date"])
  
  gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='LDNDC'
                        &year(N2O_ghaday_piv$date) %in% 1991:2014,] %>%
    ggplot(aes(x=date, y=n2o_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'
                                   &year(N2O_ghaday_piv$date) %in% 1991:2014,],
               aes(x=date, y=n2o_val, color=source), size=.5) +
    geom_segment(data=Fert[Fert$treatment=="T1"&Fert$n_rate_kg_ha>10
                           &year(Fert$date) %in% 1991:2014,],
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
    ylab(expression('N'[2]*'O Emissions (g N ha ' ^-1*'day ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("LDNDC","Observed","Fertilizer"),
                       values=cbPalette9[c(8,1,7)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNG
  

  gMG <- CH4_ghaday_piv[CH4_ghaday_piv$source=='LDNDC'
                        &year(CH4_ghaday_piv$date) %in% 1992:2014,] %>%
    ggplot(aes(x=date, y=ch4_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=CH4_ghaday_piv[CH4_ghaday_piv$source=='Observed'
                                   &year(CH4_ghaday_piv$date) %in% 1992:2014,],
               aes(x=date, y=ch4_val, color=source)) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (g C ha ' ^-1*'day ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"CH"["4"]*" Emissions"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("LDNDC","Observed","Fertilizer"),
                       values=cbPalette9[c(8,1,7)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMG
  
  # gCI <- DayCI_gm2yr[DayCI_gm2yr$year <= experiment_end_year,] %>%
  #   ggplot(aes(x=year, y=base), show.legend=TRUE) +
  #   geom_line(show.legend=TRUE) +
  #   xlab("Year") +
  #   ylab(expression('C input (g C m' ^-2*' yr' ^-1*')')) +
  #   ggtitle(paste(site_name,"Soil C Input"),
  #           paste0("Scenario: ",scenario_descriptor)) +
  #   # scale_color_manual(labels=c("LDNDC","Observed"),
  #   #                    values=cbPalette9[c(8,1)]) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gCI
  # 
  # gNI <- DayNI_gm2yr[DayNI_gm2yr$year <= experiment_end_year,] %>%
  #   ggplot(aes(x=year, y=base), show.legend=TRUE) +
  #   geom_line(show.legend=TRUE) +
  #   xlab("Year") +
  #   ylab(expression('N input (g C m' ^-2*' yr' ^-1*')')) +
  #   ylim(0,12) +
  #   ggtitle(paste(site_name,"Soil N Input"),
  #           paste0("Scenario: ",scenario_descriptor)) +
  #   # scale_color_manual(labels=c("LDNDC","Observed"),
  #   #                    values=cbPalette9[c(8,1)]) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gNI
  # 
  # gNH4 <- Day_soiln[Day_soiln$year <= experiment_end_year,] %>%
  #   ggplot(aes(x=year, y=ammonium)) +
  #   geom_line() +
  #   xlab("Year") +
  #   ylab(expression('NH4 ppm per day')) +  
  #   ylim(0,125) +
  #   ggtitle(bquote(.(site_name)~"Soil NH"["4"]*" - top 10 cm"),
  #           paste0("Scenario: ",scenario_descriptor)) +
  #   # scale_color_manual(labels=c("LDNDC","Observed"),
  #   #                    values=cbPalette9[c(8,1)]) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gNH4
  # 
  # gNO3 <- Day_soiln[Day_soiln$year <= experiment_end_year,] %>%
  #   ggplot(aes(x=year, y=NO3_ppm)) +
  #   geom_line() +
  #   xlab("Year") +
  #   ylab(expression('NO3 ppm per day')) +  
  #   ylim(0,125) +
  #   ggtitle(bquote(.(site_name)~"Soil NO"["3"]*" - top 20 cm"),
  #           paste0("Scenario: ",scenario_descriptor)) +
  #   # scale_color_manual(labels=c("LDNDC","Observed"),
  #   #                    values=cbPalette9[c(8,1)]) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gNO3
  # 
  # # explain N2O emissions with SAT, DUL, sw, NO3 and N2O
  # transform_factor <- 200
  # 
  # gN2O_expl <- ggplot() +
  #   geom_hline(aes(yintercept=0.26, color=cbPalette9[6]), linewidth=1) + # DUL, from APSIM surface layer 0-20 cm
  #   geom_line(data=Day_soiln[Day_soiln$year %in% 2010:2015,],
  #             aes(x=date,y=NO3_kgha/transform_factor, color=cbPalette9[4]), linewidth=1) +
  #   geom_line(data=N2O_ghaday[year(N2O_ghaday$date) %in% 2010:2015,],
  #             aes(x=date, y=LDNDC/1000, color=cbPalette9[8]), linewidth=1) +
  #   geom_line(data=SoilMoist_VSM[SoilMoist_VSM$year %in% 2010:2015,],
  #             aes(x=date, y=LDNDC, color=cbPalette9[2]), linewidth=1) +
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
  # 
  # gNG_20ghd <- N2O_ghaday_piv[N2O_ghaday_piv$source==model_name &
  #                               year(N2O_ghaday_piv$date) %in% year(ObsGas$date),] %>%
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
  #   scale_color_manual(labels=c(model_name,"Observed","Fertilizer"),
  #                      values=cbPalette9[c(8,1,7)]) +
  #   theme_classic(base_family = "serif", base_size = 15) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gNG_20ghd
  
  ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gMY,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Maize_hist_yield_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gMhY,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gSY,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soybean_hist_yield_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gShY,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gWY,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Wheat_hist_yield_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gWhY,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gC,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gT,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gM,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_N2O_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gNG,
         width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_CH4_comparison_exp_",scenario_name,"_LDNDC.jpg"),plot=gMG,
         width=6, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"calib_C_input_exp_",scenario_name,"_LDNDC.jpg"),plot=gCI,
  #        width=9, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"calib_N_input_exp_",scenario_name,"_LDNDC.jpg"),plot=gNI,
  #        width=9, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"calib_NH4_input_exp_",scenario_name,"_LDNDC.jpg"),plot=gNH4,
  #        width=9, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"calib_NO3_input_exp_",scenario_name,"_LDNDC.jpg"),plot=gNO3,
  #        width=9, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"calib_N2O_comparison_20ghd_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_20ghd,
  #        width=9, height=6, dpi=300)
  
  
  
  #**********************************************************************
  
  # 1:1 graphs --------------------------------------------------------------
  
  
  # corn
  MYfit <- lm(LDNDC ~ Observed, data = MaizeYld_Mgha)
  MYfit_coef <- coef(MYfit)
  MYfit_r2 <- round(summary(MYfit)$r.squared,2)
  
  MY_rmse_error <- MaizeYld_Mgha$Observed-MaizeYld_Mgha$LDNDC
  MY_rmse <- round(sqrt(mean(MY_rmse_error^2,na.rm=TRUE)),2)
  
  gMY_121 <- MaizeYld_Mgha %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=MYfit_coef[1], slope=MYfit_coef[2], color="blue") +
    annotate("text",x=1,y=0,label=paste0("R^2=",MYfit_r2)) +
    ggtitle(bquote(.(site_name)~"Maize Yield (Mg ha" ^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gMY_121
  
  ## soy
  SYfit <- lm(LDNDC ~ Observed, data = SoyYld_Mgha)
  SYfit_coef <- coef(SYfit)
  SYfit_r2 <- round(summary(SYfit)$r.squared,2)
  
  SY_rmse_error <- SoyYld_Mgha$Observed-SoyYld_Mgha$LDNDC
  SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)
  
  gSY_121 <- SoyYld_Mgha %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
    annotate("text",x=1.5,y=3.5,label=paste0("R^2=",SYfit_r2)) +
    annotate("text",x=1.5,y=3,label=paste0("Slope=",round(SYfit_coef[2],2))) +
    ggtitle(bquote(.(site_name)~"Soybean Yield (Mg ha" ^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gSY_121
  
  
  ## wheat
  WYfit <- lm(LDNDC ~ Observed, data = WheatYld_Mgha)
  WYfit_coef <- coef(WYfit)
  WYfit_r2 <- round(summary(WYfit)$r.squared,2)
  
  WY_rmse_error <- WheatYld_Mgha$Observed-WheatYld_Mgha$LDNDC
  WY_rmse <- round(sqrt(mean(WY_rmse_error^2,na.rm=TRUE)),2)
  
  gWY_121 <- WheatYld_Mgha %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=WYfit_coef[1], slope=WYfit_coef[2], color="blue") +
    #annotate("text",x=3,y=3,label=paste0("R^2=",WYfit_r2)) +
    annotate("text",x=1,y=1,label=substitute(paste(R^2, '=', r2, sep=""),list(r2=WYfit_r2))) +
    #expression('N'[2]*'O Emissions (g ha ' ^-1*' day '^-1*')')
    ggtitle(bquote(.(site_name)~"Wheat Yield (Mg ha" ^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gWY_121
  
  
  ## soil organic C
  Cfit <- lm(LDNDC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$LDNDC
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
  
  gC_121 <- Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date,] %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
    ggtitle(bquote(.(site_name)~"SOC Stock (Mg C ha" ^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gC_121
  
  ## soil temperature
  Tfit <- lm(LDNDC ~ Observed, data = SoilTemp_C)
  Tfit_coef <- coef(Tfit)
  Tfit_r2 <- round(summary(Tfit)$r.squared,2)
  
  T_rmse_error <- SoilTemp_C$Observed-SoilTemp_C$LDNDC
  T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)
  
  gT_121 <- SoilTemp_C %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
    annotate("text",x=-3,y=-10,label=paste0("R^2=",Tfit_r2)) +
    ggtitle(bquote(.(site_name)~"Soil Temperature ("*degree*"C)"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gT_121
  
  ## soil moisture
  Mfit <- lm(LDNDC ~ Observed, data = SoilMoist_VSM)
  Mfit_coef <- coef(Mfit)
  Mfit_r2 <- round(summary(Mfit)$r.squared,2)
  
  M_rmse_error <- SoilMoist_VSM$Observed-SoilMoist_VSM$LDNDC
  M_rmse <- round(sqrt(mean(M_rmse_error^2,na.rm=TRUE)),2)
  
  gM_121 <- SoilMoist_VSM %>%
    ggplot(aes(x=Observed, y=LDNDC, 
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T), 
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=Mfit_coef[1], slope=Mfit_coef[2], color="blue") +
    annotate("text",x=-3,y=-10,label=paste0("R^2=",Mfit_r2)) +
    ggtitle(paste0(site_name," Volumetric Soil Moisture"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gM_121
  
  ##
  Nfit <- lm(LDNDC ~ Observed, data = N2O_ghaday)
  Nfit_coef <- coef(Nfit)
  Nfit_r2 <- round(summary(Nfit)$r.squared,2)
  
  N_rmse_error <- N2O_ghaday$Observed-N2O_ghaday$LDNDC
  N_rmse <- round(sqrt(mean(N_rmse_error^2,na.rm=TRUE)),2)
  
  gNG_121 <- N2O_ghaday[N2O_ghaday$Observed!=0,] %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=Nfit_coef[1], slope=Nfit_coef[2], color="blue") +
    annotate("text",x=-3,y=-10,label=paste0("R^2=",Nfit_r2)) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gNG_121
  
  ##
  Hfit <- lm(LDNDC ~ Observed, data = CH4_ghaday)
  Hfit_coef <- coef(Hfit)
  Hfit_r2 <- round(summary(Hfit)$r.squared,2)
  
  H_rmse_error <- CH4_ghaday$Observed-CH4_ghaday$LDNDC
  H_rmse <- round(sqrt(mean(H_rmse_error^2,na.rm=TRUE)),2)
  
  gMG_121 <- CH4_ghaday[CH4_ghaday$Observed!=0,] %>%
    ggplot(aes(x=Observed, y=LDNDC,
               xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
               ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=Hfit_coef[1], slope=Hfit_coef[2], color="blue") +
    ggtitle(bquote(.(site_name)~"CH"["4"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +  
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(hjust = 0.5))
  
  gMG_121
  
  # ##
  # GCfit <- lm(LDNDC ~ Observed, data = grainC_gm2)
  # GCfit_coef <- coef(GCfit)
  # GCfit_r2 <- round(summary(GCfit)$r.squared,2)
  # 
  # gGC_121 <- grainC_gm2[grainC_gm2$year %in% experiment_year_range,] %>%
  #   ggplot(aes(x=Observed, y=LDNDC,
  #              xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
  #              ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T),
  #              color=crop)) +
  #   geom_point() +
  #   geom_abline() +
  #   geom_abline(intercept=GCfit_coef[1], slope=GCfit_coef[2], color="blue") +
  #   annotate("text",x=1,y=0,label=paste0("R^2=",GCfit_r2)) +
  #   #  geom_abline(intercept=PNfit[1], slope=PNfit[2], color="blue") +
  #   ggtitle(expression('Grain C (g C m' ^-2*')')) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line(),
  #         plot.title = element_text(hjust = 0.5))
  # 
  # gGC_121
  # 
  # ##
  # SCfit <- lm(LDNDC ~ Observed, data = stoverC_gm2)
  # SCfit_coef <- coef(SCfit)
  # SCfit_r2 <- round(summary(SCfit)$r.squared,2)
  # 
  # gSC_121 <- stoverC_gm2[stoverC_gm2$year %in% experiment_year_range,] %>%
  #   ggplot(aes(x=Observed, y=LDNDC,
  #              xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
  #              ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T),
  #              color=crop)) +
  #   geom_point() +
  #   geom_abline() +
  #   geom_abline(intercept=SCfit_coef[1], slope=SCfit_coef[2], color="blue") +
  #   annotate("text",x=1,y=0,label=paste0("R^2=",SCfit_r2)) +
  #   #  geom_abline(intercept=PNfit[1], slope=PNfit[2], color="blue") +
  #   ggtitle(expression('Stover C (g C m' ^-2*')')) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line(),
  #         plot.title = element_text(hjust = 0.5))
  # 
  # gSC_121
  # 
  # ##
  # GNfit <- lm(LDNDC ~ Observed, data = grainN_gm2)
  # GNfit_coef <- coef(GNfit)
  # GNfit_r2 <- round(summary(GNfit)$r.squared,2)
  # 
  # #PNfit <- coef(lm(LDNDC ~ Observed, data = grainN_gm2))
  # 
  # gGN_121 <- grainN_gm2[grainN_gm2$year %in% experiment_year_range,] %>%
  #   ggplot(aes(x=Observed, y=LDNDC,
  #              xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
  #              ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T),
  #              color=crop)) +
  #   geom_point() +
  #   geom_abline() +
  #   geom_abline(intercept=GNfit_coef[1], slope=GNfit_coef[2], color="blue") +
  #   annotate("text",x=1,y=0,label=paste0("R^2=",GNfit_r2)) +
  # #  geom_abline(intercept=PNfit[1], slope=PNfit[2], color="blue") +
  #   ggtitle(expression('Grain N (g N m' ^-2*')')) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line(),
  #         plot.title = element_text(hjust = 0.5))
  # 
  # gGN_121
  # 
  # ##
  # SNfit <- lm(LDNDC ~ Observed, data = stoverN_gm2)
  # SNfit_coef <- coef(SNfit)
  # SNfit_r2 <- round(summary(SNfit)$r.squared,2)
  # 
  # #PNfit <- coef(lm(LDNDC ~ Observed, data = stoverN_gm2))
  # 
  # gSN_121 <- stoverN_gm2[stoverN_gm2$year %in% experiment_year_range,] %>%
  #   ggplot(aes(x=Observed, y=LDNDC,
  #              xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
  #              ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T),
  #              color=crop)) +
  #   geom_point() +
  #   geom_abline() +
  #   geom_abline(intercept=SNfit_coef[1], slope=SNfit_coef[2], color="blue") +
  #   annotate("text",x=1,y=0,label=paste0("R^2=",SNfit_r2)) +
  #   #  geom_abline(intercept=PNfit[1], slope=PNfit[2], color="blue") +
  #   ggtitle(expression('Stover N (g N m' ^-2*')')) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line(),
  #         plot.title = element_text(hjust = 0.5))
  # 
  # gSN_121
  # 
  # ##
  # GCNfit <- lm(LDNDC ~ Observed, data = grainCN)
  # GCNfit_coef <- coef(GCNfit)
  # GCNfit_r2 <- round(summary(GCNfit)$r.squared,2)
  # 
  # #PNfit <- coef(lm(LDNDC ~ Observed, data = stoverN_gm2))
  # 
  # gGCN_121 <- grainCN[grainCN$year %in% experiment_year_range,] %>%
  #   ggplot(aes(x=Observed, y=LDNDC,
  #              xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
  #              ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T),
  #              color=crop)) +
  #   geom_point() +
  #   geom_abline() +
  #   geom_abline(intercept=GCNfit_coef[1], slope=GCNfit_coef[2], color="blue") +
  #   annotate("text",x=1,y=0,label=paste0("R^2=",GCNfit_r2)) +
  #   #  geom_abline(intercept=PNfit[1], slope=PNfit[2], color="blue") +
  #   ggtitle(expression('Grain C:N ratio')) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line(),
  #         plot.title = element_text(hjust = 0.5))
  # 
  # gGCN_121
  # 
  # ##
  # SCNfit <- lm(LDNDC ~ Observed, data = grainCN)
  # SCNfit_coef <- coef(SCNfit)
  # SCNfit_r2 <- round(summary(SCNfit)$r.squared,2)
  # 
  # #PNfit <- coef(lm(LDNDC ~ Observed, data = stoverN_gm2))
  # 
  # gSCN_121 <- stoverCN[stoverCN$year %in% experiment_year_range,] %>%
  #   ggplot(aes(x=Observed, y=LDNDC,
  #              xmin=min(Observed, LDNDC, na.rm=T), xmax=max(Observed, LDNDC, na.rm=T),
  #              ymin=min(Observed, LDNDC, na.rm=T), ymax=max(Observed, LDNDC, na.rm=T),
  #              color=crop)) +
  #   geom_point() +
  #   geom_abline() +
  #   geom_abline(intercept=SCNfit_coef[1], slope=SCNfit_coef[2], color="blue") +
  #   annotate("text",x=1,y=0,label=paste0("R^2=",SCNfit_r2)) +
  #   #  geom_abline(intercept=PNfit[1], slope=PNfit[2], color="blue") +
  #   ggtitle(expression('Stover C:N ratio')) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line(),
  #         plot.title = element_text(hjust = 0.5))
  # 
  # gSCN_121
  
  ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gMY_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gSY_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gWY_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gC_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gT_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gM_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_N2O_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gNG_121, width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_CH4_comparison_1to1_",scenario_name,"_LDNDC.jpg"),
         plot=gMG_121, width=6, height=6, dpi=300)
  
  
  
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
                         SOC_obsmod_diff_Mgha,
                         Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse,
                         Mfit_coef[2], Mfit_coef[1], Mfit_r2, M_rmse,
                         Nfit_coef[2], Nfit_coef[1], Nfit_r2, N_rmse,
                         N2O_obsmod_diff_gha,
                         Hfit_coef[2], Hfit_coef[1], Hfit_r2, H_rmse,
                         CH4_obsmod_diff_gha,
                         NA, NA, NA, NA, # Cotton
                         NA,
                         NA, NA, NA, NA, # Sorghum
                         NA,
                         NA, NA, NA, # maize, soybean, wheat cultivars
                         NA, # cotton cultivar
                         NA # sorghum cultivar
  )
  
  
  source("p_Edit_calib_file.R")
  p_Edit_calib_file(calib_log_tab,model_name,scenario_name)
  
}) # end suppressMessages