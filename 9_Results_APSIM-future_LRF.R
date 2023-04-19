#title: "9_Results_APSIM-future.R"
#author: "Ellen Maas"
#date: "7/22/2022"
#output: html_document
#description: "Runs all graphs for the APSIM simulation at KBS, MI."

suppressMessages({
  
  print("Starting 9_Results_APSIM-future.R")
  
  library(apsimx)
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  
  # Temporal graphs
  # experimental -> future period
  
  ## Cotton
  
  CYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = CottonYld_Mgha[CottonYld_Mgha$year>end_exp_period_year,]))
  CYfit_Obs <- coef(lm(Observed ~ year, 
                       data = CottonYld_Mgha[CottonYld_Mgha$year %in% experiment_year_range,]))
  CYxs <- c(end_exp_period_year+1, end_fut_period_year)
  CYys <- cbind(1, CYxs) %*% CYfit_APSIM
  CYobsxs <- c(experiment_start_year, experiment_end_year)
  CYobsys <- cbind(1, CYobsxs) %*% CYfit_Obs
  
  gCY <- CottonYld_Mgha_piv[CottonYld_Mgha_piv$source %in% c("APSIM","Observed"),] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Cotton Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
#    geom_abline(intercept=CYfit_APSIM[1], slope=CYfit_APSIM[2], color="orange") +
    geom_segment(aes(x = CYxs[1], xend = CYxs[2], y = CYys[1], yend = CYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = CYobsxs[1], xend = CYobsxs[2], y = CYobsys[1], yend = CYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCY
  
  gChY <- CottonYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Cotton Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    #    geom_abline(intercept=CYfit_APSIM[1], slope=CYfit_APSIM[2], color="orange") +
    #geom_segment(aes(x = CYxs[1], xend = CYxs[2], y = CYys[1], yend = CYys[2]), color=cbPalette9[8]) +
    #geom_segment(aes(x = CYobsxs[1], xend = CYobsxs[2], y = CYobsys[1], yend = CYobsys[2]), color=cbPalette9[1]) +
    geom_vline(xintercept=1987,linetype=2) +
    geom_vline(xintercept=2003,linetype=2) +
    geom_vline(xintercept=2010,linetype=2) +
    scale_color_manual(labels=c("APSIM","Historical","Observed"),
                       values=cbPalette9[c(8,4,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gChY
  
  if(mgmt_scenario_grp!=7) {
  ## Sorghum
  
  SYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = SorghumYld_Mgha[SorghumYld_Mgha$year>end_exp_period_year,]))
  SYfit_Obs <- coef(lm(Observed ~ year, 
                       data = SorghumYld_Mgha[SorghumYld_Mgha$year %in% experiment_year_range,]))
  SYxs <- c(end_exp_period_year+1, end_fut_period_year)
  SYys <- cbind(1, SYxs) %*% SYfit_APSIM
  SYobsxs <- c(experiment_start_year, experiment_end_year)
  SYobsys <- cbind(1, SYobsxs) %*% SYfit_Obs
  
  gSY <- SorghumYld_Mgha_piv[SorghumYld_Mgha_piv$source %in% c("APSIM","Observed"),] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Sorghum Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = SYxs[1], xend = SYxs[2], y = SYys[1], yend = SYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = SYobsxs[1], xend = SYobsxs[2], y = SYobsys[1], yend = SYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSY

  gShY <- SorghumYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Sorghum Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    #geom_segment(aes(x = SYxs[1], xend = SYxs[2], y = SYys[1], yend = SYys[2]), color=cbPalette9[8]) +
    #geom_segment(aes(x = SYobsxs[1], xend = SYobsxs[2], y = SYobsys[1], yend = SYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Historical","Observed"),
                       values=cbPalette9[c(8,4,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gShY
  
  }

  ## SOC
  
  Cfit_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha))
  if(mgmt_scenario_grp==3) {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                              Cstock_Mgha$year >= experiment_start_year,]))
  } else {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  }
  
  Cxs <- c(end_exp_period_year+1, end_fut_period_year)
  Cys <- cbind(1, Cxs) %*% Cfit_APSIM
  Cobsxs <- c(experiment_start_year, experiment_end_year)
  Cobsys <- cbind(1, Cobsxs) %*% Cfit_Obs
  
  gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year>=experiment_start_year,] %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
    # geom_abline(intercept=Cfit_APSIM[1], slope=Cfit_APSIM[2], color="orange") +
    # geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
    ggtitle(paste(site_name,"Soil Organic Carbon"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = Cxs[1], xend = Cxs[2], y = Cys[1], yend = Cys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = Cobsxs[1], xend = Cobsxs[2], y = Cobsys[1], yend = Cobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  
  ## soil temp
  
  gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='APSIM' & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
    ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed' & SoilTemp_C_piv$year %in% ObsTemp$year,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    ggtitle(paste0(site_name," Soil Temperature"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  # gT_calib <- SoilTemp_C_piv_calib[SoilTemp_C_piv_calib$source=='APSIM' & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
  #   ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  #   geom_point() +
  #   geom_point(data=SoilTemp_C_piv_calib[SoilTemp_C_piv_calib$source=='Observed' & SoilTemp_C_piv$year %in% ObsTemp$year,],
  #              aes(x=date, y=temp_val, color=source)) +
  #   ggtitle(paste0("APSIM and Observed Soil Temperature with ",soil_temp_bias,
  #                  " deg C correction")) +
  #   xlab("Year") +
  #   ylab(expression('Soil temperature (' ^o*'C)')) +
  #   ggtitle(paste0(site_name," Soil Temperature with ",soil_temp_bias,
  #                  " deg C correction"),paste0("Scenario: ",scenario_descriptor_full)) +
  #   scale_color_manual(labels=c("APSIM","Observed"),
  #                      values=cbPalette9[c(8,1)]) +
  #   theme_classic(base_family = "serif", base_size = 15) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gT_calib
  
  gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM' & SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
    ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed' & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
               aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture (%)") +
    ggtitle(paste0(site_name," Soil Moisture"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  # gM_calib <- SoilMoist_VSM_piv_calib[SoilMoist_VSM_piv_calib$source=='APSIM' & 
  #                                       SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
  #   ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
  #   geom_point() +
  #   geom_point(data=SoilMoist_VSM_piv_calib[SoilMoist_VSM_piv_calib$source=='Observed' & 
  #                                             SoilMoist_VSM_piv$year %in% ObsVSM$year,],
  #              aes(x=date, y=h2o_val, color=source)) +
  #   xlab("Year") +
  #   ylab("Volumetric soil moisture (%)") +
  #   ggtitle(paste0(site_name," Volumetric soil moisture with ",soil_moist_bias,"% correction"),
  #           paste0("Scenario: ",scenario_descriptor_full)) +
  #   scale_color_manual(labels=c("APSIM","Observed"),
  #                      values=cbPalette9[c(8,1)]) +
  #   theme_classic(base_family = "serif", base_size = 15) +
  #   theme(panel.background = element_blank(),
  #         axis.line = element_line(),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # gM_calib
  
  # gB <- SoilBD_gcc_piv %>%
  #   ggplot(aes(x=source,y=bd_val, fill=factor(year))) +
  #   geom_col(position="dodge") +
  #   ylab(expression('Bulk density (g cc' ^-1*')')) +
  #   labs(fill="Year") +
  #   theme_classic(base_family = "serif", base_size = 15) +
  #   theme(panel.background = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line = element_line())
  # 
  # gB
  
  # Daily N2O
  
  gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM',] %>%
    ggplot(aes(x=date, y=n2o_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed',],
               aes(x=date, y=n2o_val, color=source)) +
    geom_segment(data=Fert[Fert$treatment==treatment & Fert$totalN_kgha>10,],
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
    ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'day ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                       values=cbPalette9[c(8,1,7)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNG

  # Annual N2O
  
  NGfit_APSIM <- coef(lm(N2OEmissions_ghayr ~ year, 
                         data = APSIMGN_ann_gha[APSIMGN_ann_gha$year>end_exp_period_year,]))
  NGxs <- c(end_exp_period_year+1, end_fut_period_year)
  NGys <- cbind(1, NGxs) %*% NGfit_APSIM

  gNGann <- N2O_ghayr_piv %>%
    ggplot(aes(x=year, y=n2o_val, color=source)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Annual Cumulative N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = NGxs[1], xend = NGxs[2], y = NGys[1], yend = NGys[2]), color=cbPalette9[8]) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNGann
  
  ggsave(filename=paste0(results_path,"Cotton_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gCY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Cotton_hist_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gChY, width=9, height=6, dpi=300)
  if(mgmt_scenario_grp!=7) {
    ggsave(filename=paste0(results_path,"Sorghum_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gSY, width=9, height=6, dpi=300)
    ggsave(filename=paste0(results_path,"Sorghum_hist_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
           plot=gShY, width=9, height=6, dpi=300)
  }
  ggsave(filename=paste0(results_path,"SOC_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gC, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Temp_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gT, width=9, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"Soil_Temp_comparison_calib_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
  #        plot=gT_calib, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Moist_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gM, width=9, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"Soil_Moist_comparison_calib_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
  #        plot=gM_calib, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gNG, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_ann_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gNGann, width=9, height=6, dpi=300)
  
  
}) # end suppressMessages