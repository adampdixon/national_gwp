#*************************************************************
# File: 10_Model_Ensemble_Results-combined_scenarios_KBS
# Author: Ellen Maas
# Date: 8/30/2022
# Description: Assembles individual model runs and calculates
# annual and daily mean values, then graphs them. Also 
# calculates the global warming potential by scenario and 
# graphs it.
#*************************************************************
# Calls:
# f_model_coef
#*************************************************************
# Audit Log
# 8/30/2022: Created script.
# 12/22/2022: Reworked linear models to use f_model_coef, 
# loop for climate scenarios.
# 2/15/2023: Made y-axes equivalent for future change graphs.
#*************************************************************

suppressMessages({
  
print(paste0("Starting 10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)

source("f_model_coef.R")

  #*************************************************************


# Import calibration data -------------------------------------------------

crop_calib_output_df_piv <- read.table(file=paste0(results_path,"calib_crop_df_piv.csv"),
                                       header=TRUE,sep=",")  
crop_calib_output_df_piv$Source <- crop_calib_output_df_piv$Model
soc_calib_output_df_piv <- read.table(file=paste0(results_path,"calib_soc_df_piv.csv"),
                                      header=TRUE,sep=",")  
soc_calib_output_df_piv$Source <- soc_calib_output_df_piv$Model
# soc_trendlines_df <- read.table(file=paste0(results_path,"calib_soc_trendline_piv.csv"),
#                                 header=TRUE,sep=",")  
# soc_trendlines_df$Source <- soc_trendlines_df$Model
  
  # Calibration graphs ------------------------------------------------------

  gY_calib <- crop_calib_output_df_piv[crop_calib_output_df_piv$year %in% experiment_year_range,] %>%
    ggplot(aes(x=year, y=yield_val, color=Source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    ylim(0,3) +
    ggtitle(paste(site_name,"Crop Yield Calibration")) +
    geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                  width=.5) + # Width of the error bars
    geom_smooth(method='lm', formula= y~x, se=FALSE, aes(colour = Model)
                ,linewidth=0.75) +
    scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                       values=cbPalette9[c(8,2,1)]) +
    facet_grid(crop~treatment_scen) +
    theme_classic(base_family = "serif", base_size = 16) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gY_calib
  
  gSOC_calib <- soc_calib_output_df_piv[soc_calib_output_df_piv$year %in% experiment_year_range,] %>%
    ggplot(aes(x=year, y=C_val, color=Source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('SOC (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Soil Organic Carbon Calibration")) +
    geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                  width=.5) +  # Width of the error bars
    geom_smooth(method='lm', formula= y~x, se=FALSE, aes(colour = Model)
                ,linewidth=0.75) +
    # stat_smooth(
    #   data = soc_trendlines_df, 
    #   aes(linetype = Fit, y=value), 
    #   #color="black", 
    #   method = lm,
    #   se = FALSE,
    #   show.legend = TRUE,
    #   fullrange=TRUE
    # ) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                       values=cbPalette9[c(8,2,6,1,3)]) +
    facet_wrap(~treatment_scen,nrow=1) +
    theme_classic(base_family = "serif", base_size = 16) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSOC_calib
  
  ggsave(filename=paste0(results_path,"pub_Crop_yield_calibration.jpg"),
         plot=gY_calib, width=19, height=9, dpi=300)
  ggsave(filename=paste0(results_path,"pub_SOC_calibration.jpg"),
         plot=gSOC_calib, width=19, height=9, dpi=300)
  
  #*************************************************************

    # Import summarized data -------------------------------------------------


# global warming potential
## calculated by mean of SOC, N2O, CH4 among all models by 2100/2050/etc,
## then CO2 equivalent in 100-yr timeframe.
## then calculate emissions - sequestration: N2O + CH4 - SOC
## CH4 is usually negative, so will offset emissions

## read in model summary output
summary_output_raw <- read.csv(paste0(results_path,"Summary_output.csv")) 

## get means for N2O and CH4 to calculate separately for change-over-time
scenario_gas_means <- summary_output_raw %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(mean_N2O_Mgha=round(mean(Total_N2O_kgha/1000,na.rm=T),4),
            mean_CH4_Mgha=round(mean(Total_CH4_kgha/1000,na.rm=T),4),
            sd_N2O_Mgha=round(sd(Total_N2O_kgha/1000,na.rm=T),4),
            sd_CH4_Mgha=round(sd(Total_CH4_kgha/1000,na.rm=T),4)
  ) %>%
  left_join(scenario_df[,c("scenario_name","scenario_abbrev")],by=c("Scenario_Name"="scenario_name"))


## get means for N2O and CH4 to fill in for each model which doesn't include it,
## in order to calculate GWP:
## N2O - RothC and Millennial
## CH4 - APSIM, RothC, and Millennial

ghg_means <- summary_output_raw %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(model_mean_N2O_kgha=mean(Total_N2O_kgha,na.rm=T),
            model_mean_CH4_kgha=mean(Total_CH4_kgha,na.rm=T)
  )

## now fill in missing data
summary_fillin <- left_join(summary_output_raw,
                            ghg_means,
                            by=c("Climate_Scenario","Mgmt_Scenario","Scenario_Name")) 
summary_fillin[is.na(summary_fillin$Total_N2O_kgha),"Total_N2O_kgha"] <-
  summary_fillin[is.na(summary_fillin$Total_N2O_kgha),"model_mean_N2O_kgha"]
summary_fillin[is.na(summary_fillin$Total_CH4_kgha),"Total_CH4_kgha"] <-
  summary_fillin[is.na(summary_fillin$Total_CH4_kgha),"model_mean_CH4_kgha"]

summary_output <- summary_fillin[,-which(names(summary_fillin) %in% 
                                           c("model_mean_N2O_kgha","model_mean_CH4_kgha"))] %>%
  mutate(N2O_Mgha=Total_N2O_kgha/1000,
         CH4_Mgha=Total_CH4_kgha/1000,
         CO2e_SOC=-SOC_Diff_Mgha*(44/12), # convert SOC to CO2e (CO2 gas), make negative 
         CO2e_N2O=N2O_Mgha*(44/28)*273, # convert N2O(N) to N2O gas, then to CO2e
         CO2e_CH4=CH4_Mgha*(16/12)*27 # convert CH4(C) to CH4 gas, then to CO2e
  ) %>%
left_join(scenario_df[,c("scenario_name","scenario_abbrev")],
          by=c("Scenario_Name"="scenario_name"))

summary_output$GWP=rowSums(summary_output[grep("^CO2e", names(summary_output))], 
                           na.rm=TRUE) # SOC negative for sequestration

# write out summary output for final analysis
write.csv(summary_output, file=paste0(results_path,"summary_output_final.csv"),
          row.names=FALSE)

scenario_means <- summary_output %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(mean_CottonYld_Mgha=round(mean(Cotton_Diff_Mgha,na.rm=T),5),
            mean_SorghumYld_Mgha=round(mean(Sorghum_Diff_Mgha,na.rm=T),5),
            mean_SOC_Mgha=round(mean(SOC_Diff_Mgha,na.rm=T),5),
            mean_N2O_Mgha=round(mean(N2O_Mgha,na.rm=T),5),
            mean_CH4_Mgha=round(mean(CH4_Mgha,na.rm=T),5),
            mean_CO2e_SOC=round(mean(CO2e_SOC,na.rm=T),5),
            mean_CO2e_N2O=round(mean(CO2e_N2O,na.rm=T),5),
            mean_CO2e_CH4=round(mean(CO2e_CH4,na.rm=T),5),
            mean_GWP=round(mean(GWP,na.rm=T),5),
            sd_CottonYld_Mgha=round(sd(Cotton_Diff_Mgha,na.rm=T),5),
            sd_SorghumYld_Mgha=round(sd(Sorghum_Diff_Mgha,na.rm=T),5),
            sd_SOC_Mgha=round(sd(SOC_Diff_Mgha,na.rm=T),5),
            sd_N2O_Mgha=round(sd(N2O_Mgha,na.rm=T),5),
            sd_CH4_Mgha=round(sd(CH4_Mgha,na.rm=T),5),
            sd_CO2e_SOC=round(sd(CO2e_SOC),5),
            sd_CO2e_N2O=round(sd(CO2e_N2O),5),
            sd_CO2e_CH4=round(sd(CO2e_CH4),5),
            sd_GWP=round(sd(GWP,na.rm=T),5)
  ) %>%
  left_join(scenario_df[,c("scenario_name","scenario_abbrev")],
            by=c("Scenario_Name"="scenario_name"))



# write out scenario means
write.csv(scenario_means, file=paste0(results_path,"scenario_means.csv"),
          row.names=FALSE)

gwp_means_piv <- pivot_longer(scenario_means,c(-Climate_Scenario,
                                               -Mgmt_Scenario,
                                               -Scenario_Name,
                                               -scenario_abbrev,
),
names_to="source",
values_to="vals")

annual_results <- data.frame()
# loop through csvs to get the scenarios with options (like groups 4 and 5)
for (i in clim_nums) { # climate scenarios
  for (j in mgmt_grps) { # management groups
    max_options <- if_else(j==4,4,
                   if_else(j==5,3,
                   if_else(j==6,5,1)))
    for (k in 1:max_options) { # management options
      if(j!=6) {
        for (l in c("APSIM","Daycent","Millennial","RothC")) {
          scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
          read_dat <- read.csv(paste0(results_path,"Annual_results_compilation_",scen_nm,
                                      "_",l,".csv"))
          annual_results <- rbind(annual_results,read_dat)
        } # end for - models
      } else {
        scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
        read_dat <- read.csv(paste0(results_path,"Annual_results_compilation_",scen_nm,
                                    "_APSIM.csv"))
        annual_results <- rbind(annual_results,read_dat)
      } # end if - management group != 6
    } # end for - management group options
  } # end for - management groups
} # end for-climate scenarios

# fill-in NA yields with 0
# annual_results[is.na(annual_results$CottonYld_Mgha),"CottonYld_Mgha"] <- 0
# annual_results[is.na(annual_results$SorghumYld_Mgha),"SorghumYld_Mgha"] <- 0
# annual_results[is.na(annual_results$WheatYld_Mgha),"WheatYld_Mgha"] <- 0

# add scenario abbreviation
annual_results <- left_join(annual_results, 
                            scenario_df[,c("scenario_name","scenario_abbrev")],
                            by="scenario_name")

write.csv(annual_results, file=paste0(results_path,"annual_results.csv"),
          row.names=FALSE)

# mean annual results, by scenario
mean_annual_results <- annual_results[annual_results<end_fut_period_year,] %>%
  group_by(year,scenario_name,climate_scenario_num,mgmt_scenario_grp_num,
           mgmt_scenario_opt_num,scenario_abbrev) %>%
  summarize(CottonYld_Mgha=round(mean(CottonYld_Mgha,na.rm=T),3),
            SorghumYld_Mgha=round(mean(SorghumYld_Mgha,na.rm=T),3),
            SOC_Mgha=round(mean(SOC_Mgha,na.rm=T),3)
  )
mean_annual_results <- mean_annual_results[!is.na(mean_annual_results$year),]

write.csv(mean_annual_results, file=paste0(results_path,"mean_annual_results.csv"),
          row.names=FALSE)


daily_results <- data.frame()
# loop through csvs to get the scenarios with options (like groups 4 and 5)
for (i in clim_nums) { # climate scenarios
  for (j in mgmt_grps) { # management groups
    max_options <- if_else(j==4,4,
                           if_else(j==5,3,
                                   if_else(j==6,5,1)))
    for (k in 1:max_options) { # management options
      if(j!=6) {
        for (l in c("APSIM","Daycent")) {
          scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
          read_dat <- read.csv(paste0(results_path,"Daily_results_compilation_",scen_nm,
                                      "_",l,".csv"))
          daily_results <- rbind(daily_results,read_dat)
        } # end for - models
      } else {
        scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
        read_dat <- read.csv(paste0(results_path,"Daily_results_compilation_",scen_nm,
                                    "_APSIM.csv"))
        daily_results <- rbind(daily_results,read_dat)
      } # end if - management group != 6
    } # end for - mgmt options
  } # end for - management groups
} # end for - climate scenarios

# add N2O and CH4 conversions to kgha
daily_results$N2O_cum_kgha <- daily_results$N2O_cum_gha/1000
daily_results$CH4_cum_kgha <- daily_results$CH4_cum_gha/1000

# add scenario abbreviation
daily_results <- left_join(daily_results, 
                           scenario_df[,c("scenario_name","scenario_abbrev")],
                           by="scenario_name")


write.csv(daily_results, file=paste0(results_path,"daily_results.csv"),
          row.names=FALSE)

# mean daily results, by scenario
mean_daily_results <- daily_results[daily_results$year<end_fut_period_year,] %>%
  group_by(year,date,dayofyear,scenario_name,climate_scenario_num,
           mgmt_scenario_grp_num,mgmt_scenario_opt_num,scenario_abbrev) %>%
  summarize(N2O_emit_gha=round(mean(N2O_emit_gha,na.rm=T),5),
            N2O_cum_gha=round(mean(N2O_cum_gha,na.rm=T),5),
            N2O_cum_kgha=N2O_cum_gha/1000,
            CH4_net_gha=round(mean(CH4_net_gha,na.rm=T),5),
            CH4_cum_gha=round(mean(CH4_cum_gha,na.rm=T),5),
            CH4_cum_kgha=CH4_cum_gha/1000
  )


write.csv(mean_daily_results, file=paste0(results_path,"mean_daily_results.csv"),
          row.names=FALSE)

#*************************************************************

# Combine scenarios with multiple options ---------------------------------
#one set of graphs per climate scenario

for(clim_num in clim_nums) {
  climate_desc <-   if_else(clim_num=="1","Baseline",
                    if_else(clim_num=="2","GFDL_ESM4 Low",
                    if_else(clim_num=="3","GFDL_ESM4 High",
                    if_else(clim_num=="4","UKESM1-0-LL Low",
                    if_else(clim_num=="5","UKESM1-0-LL High",
                    "Missing Descriptor")))))
  

  ## Annual graphs-combined scenarios

  ### Mean of all scenarios

  #### Cotton

  gAllCYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$CottonYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="CottonYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="MYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllCYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

  gAllCYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$CottonYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Cotton Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                color=g$data[[1]]$colour[1]) +
    geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                color=g$data[[1]]$colour[2]) +
    geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                color=g$data[[1]]$colour[3]) +
    geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                color=g$data[[1]]$colour[4]) +
    geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                color=g$data[[1]]$colour[5]) +
    geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                color=g$data[[1]]$colour[6]) +
    geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                color=g$data[[1]]$colour[7]) +
    geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                color=g$data[[1]]$colour[8]) +
    geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                color=g$data[[1]]$colour[9]) +
    geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                color=g$data[[1]]$colour[10]) +
    geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                color=g$data[[1]]$colour[11]) +
    geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                color=g$data[[1]]$colour[12]) +
    geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                color=g$data[[1]]$colour[13]) +
    geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                color=g$data[[1]]$colour[14]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())


  gAllCYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Cotton_exp_",clim_num,".jpg"),
         plot=gAllCYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Cotton_fut_",clim_num,".jpg"),
         plot=gAllCYfut, width=9, height=6, dpi=300)


  #### Sorghum

  gAllSYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$SorghumYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num &
                                     mean_annual_results$mgmt_scenario_grp_num!=7,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllSYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results[mean_annual_results$mgmt_scenario_grp_num!=7,],
                                   modeled_element_in="SorghumYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="SYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllSYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllSYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$SorghumYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num &
                                       mean_annual_results$mgmt_scenario_grp_num!=7,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Sorghum Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                  color=g$data[[1]]$colour[1]) +
      geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                  color=g$data[[1]]$colour[2]) +
      geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                  color=g$data[[1]]$colour[3]) +
      geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                  color=g$data[[1]]$colour[4]) +
      geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                  color=g$data[[1]]$colour[5]) +
      geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                  color=g$data[[1]]$colour[6]) +
      geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                  color=g$data[[1]]$colour[7]) +
      geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                  color=g$data[[1]]$colour[8]) +
      geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                  color=g$data[[1]]$colour[9]) +
      geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                  color=g$data[[1]]$colour[10]) +
      geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                  color=g$data[[1]]$colour[11]) +
      geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                  color=g$data[[1]]$colour[12]) +
      geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                  color=g$data[[1]]$colour[13]) +
      geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                  color=g$data[[1]]$colour[14]) +
      # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Sorghum_exp_",clim_num,".jpg"),
         plot=gAllSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Sorghum_fut_",clim_num,".jpg"),
         plot=gAllSYfut, width=9, height=6, dpi=300)




  ## SOC

  gAllCexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                    mean_annual_results$SOC_Mgha != 0 &
                                    mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_line(linewidth=1) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    geom_vline(xintercept=end_exp_period_year+1,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="SOC_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="Cfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllCexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllCfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                    mean_annual_results$SOC_Mgha != 0 &
                                    mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_line(linewidth=1) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future SOC: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      # geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
      #             color=g$data[[1]]$colour[1]) +
      # geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
      #             color=g$data[[1]]$colour[2]) +
      # geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
      #             color=g$data[[1]]$colour[3]) +
      # geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
      #             color=g$data[[1]]$colour[4]) +
      # geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
      #             color=g$data[[1]]$colour[5]) +
      # geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
      #             color=g$data[[1]]$colour[6]) +
      # geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
      #             color=g$data[[1]]$colour[7]) +
      # geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
      #             color=g$data[[1]]$colour[8]) +
      # geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
      #             color=g$data[[1]]$colour[9]) +
      # geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
      #             color=g$data[[1]]$colour[10]) +
      # geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
      #             color=g$data[[1]]$colour[11]) +
      # geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
      #             color=g$data[[1]]$colour[12]) +
      # geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
      #             color=g$data[[1]]$colour[13]) +
      # geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
      #             color=g$data[[1]]$colour[14]) +
      # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  # # use this to get the colors used:
  # g <- ggplot_build(gAllfut)
  # g$data[[1]]$colour
  # [1] "#F8766D" "#D39200" "#93AA00" "#00BA38" "#00C19F" "#00B9E3" "#FF61C3" "#DB72FB" "#619CFF"

  gAllCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_SOC_exp_",clim_num,".jpg"),
         plot=gAllCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_SOC_fut_",clim_num,".jpg"),
         plot=gAllCfut, width=9, height=6, dpi=300)


  #*************************************************************

# Scenario group 4 --------------------------------------------------------


  #### Cotton yield

  gCYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$CottonYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                                   modeled_element_in="CottonYld_Mgha",
                                   model_name_in="APSIM",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=4,
                                   result_name_in="MYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="CottonYld_Mgha",
                              model_name_in="Daycent",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="MYfit")

    gCYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$CottonYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Cotton_exp_grp_4_",clim_num,".jpg"),
         plot=gCYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Cotton_fut_grp_4_",clim_num,".jpg"),
         plot=gCYfut, width=9, height=6, dpi=300)



  #### Sorghum yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$SorghumYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SorghumYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="SYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SorghumYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="SYfit")

    gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$SorghumYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Sorghum_exp_grp_4_",clim_num,".jpg"),
         plot=gSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Sorghum_fut_grp_4_",clim_num,".jpg"),
         plot=gSYfut, width=9, height=6, dpi=300)




  #### SOC

  newC <- ObsC_Mgha[ObsC_Mgha$year > land_conversion_year,] %>%
    mutate(site="Observed")

  gCexp <- ggplot() +
    geom_point(data=annual_results[annual_results$year>=experiment_start_year &
                                     annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                                     annual_results$mgmt_scenario_grp_num==4 &
                                     annual_results$climate_scenario_num==clim_num,],
               aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                       values=cbPalette9[c(8,2,6,1,3)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SOC_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="Cfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")
  RothC_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="RothC",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")
  Millennial_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="Millennial",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")

    gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                            annual_results$mgmt_scenario_grp_num==4 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    # geom_abline(intercept=Cfit_APSIM_1_41[1], slope=Cfit_APSIM_1_41[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_41[1], slope=Cfit_Daycent_1_41[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_41[1], slope=Cfit_RothC_1_41[2],
    #             color=cbPalette9[3]) +
    # geom_abline(intercept=Cfit_APSIM_1_42[1], slope=Cfit_APSIM_1_42[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_42[1], slope=Cfit_Daycent_1_42[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_42[1], slope=Cfit_RothC_1_42[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_43[1], slope=Cfit_APSIM_1_43[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_43[1], slope=Cfit_Daycent_1_43[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_43[1], slope=Cfit_RothC_1_43[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_44[1], slope=Cfit_APSIM_1_44[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_44[1], slope=Cfit_Daycent_1_44[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_44[1], slope=Cfit_RothC_1_44[2],
  #             color=cbPalette9[3]) +
  scale_color_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                     values=cbPalette9[c(8,2,6,3)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_4_",clim_num,".jpg"),
         plot=gCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_4_",clim_num,".jpg"),
         plot=gCfut, width=9, height=6, dpi=300)


  #*************************************************************

# Scenario group 5 --------------------------------------------------------


  #### Cotton yield

  gCYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$CottonYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="CottonYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="MYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="CottonYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="MYfit")

    gCYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$CottonYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Cotton_exp_grp_5_",clim_num,".jpg"),
         plot=gCYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Cotton_fut_grp_5_",clim_num,".jpg"),
         plot=gCYfut, width=9, height=6, dpi=300)



  #### Sorghum yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$SorghumYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SorghumYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="SYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SorghumYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="SYfit")

  gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$SorghumYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                color=cbPalette9[2]) +
    geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                color=cbPalette9[2]) +
    geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                color=cbPalette9[2]) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Sorghum_exp_grp_5_",clim_num,".jpg"),
         plot=gSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Sorghum_fut_grp_5_",clim_num,".jpg"),
         plot=gSYfut, width=9, height=6, dpi=300)



  #### SOC
  Cfit_RothC_1_53 <- coef(lm(SOC_Mgha ~ year,
                             data = annual_results[annual_results$year>=experiment_end_year &
                                                     annual_results$model_name=="RothC" &
                                                     annual_results$scenario_name=="1_53",]))


  gCexp <- annual_results[annual_results$year>=experiment_start_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                            annual_results$mgmt_scenario_grp_num==5 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point() +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                       values=cbPalette9[c(8,2,6,1,3)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                            annual_results$mgmt_scenario_grp_num==5 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                     values=cbPalette9[c(8,2,6,3)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_5_",clim_num,".jpg"),
         plot=gCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_5_",clim_num,".jpg"),
         plot=gCfut, width=9, height=6, dpi=300)


  #*************************************************************

# Scenario group 6 --------------------------------------------------------


  #### Cotton yield
  CYfit_APSIM_1_61 <- coef(lm(CottonYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$CottonYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_61",]))
  CYfit_APSIM_1_62 <- coef(lm(CottonYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$CottonYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_62",]))
  CYfit_APSIM_1_63 <- coef(lm(CottonYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$CottonYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_63",]))
  CYfit_APSIM_1_64 <- coef(lm(CottonYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$CottonYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_64",]))
  CYfit_APSIM_1_65 <- coef(lm(CottonYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$CottonYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_65",]))

  gCYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$CottonYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="CottonYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="MYfit")

    gCYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             #annual_results$scenario_name=="1_61" &
                             annual_results$CottonYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CottonYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Cotton Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Cotton_exp_grp_6_",clim_num,".jpg"),
         plot=gCYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Cotton_fut_grp_6_",clim_num,".jpg"),
         plot=gCYfut, width=9, height=6, dpi=300)



  #### Sorghum yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$SorghumYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp


  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SorghumYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="SYfit")

    gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$SorghumYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SorghumYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Sorghum Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Sorghum_exp_grp_6_",clim_num,".jpg"),
         plot=gSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Sorghum_fut_grp_6_",clim_num,".jpg"),
         plot=gSYfut, width=9, height=6, dpi=300)



  #### SOC

    newC <- ObsC_Mgha[ObsC_Mgha$year > land_conversion_year,] %>%
    mutate(site="Observed")

  gCexp <- ggplot() +
    geom_point(data=annual_results[annual_results$year>=experiment_start_year &
                                     annual_results$model_name %in% c("APSIM") &
                                     annual_results$mgmt_scenario_grp_num==6 &
                                     annual_results$climate_scenario_num==clim_num,],
               aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM") &
                            annual_results$mgmt_scenario_grp_num==6 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    # geom_abline(intercept=Cfit_APSIM_1_41[1], slope=Cfit_APSIM_1_41[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_41[1], slope=Cfit_Daycent_1_41[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_41[1], slope=Cfit_RothC_1_41[2],
    #             color=cbPalette9[3]) +
    # geom_abline(intercept=Cfit_APSIM_1_42[1], slope=Cfit_APSIM_1_42[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_42[1], slope=Cfit_Daycent_1_42[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_42[1], slope=Cfit_RothC_1_42[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_43[1], slope=Cfit_APSIM_1_43[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_43[1], slope=Cfit_Daycent_1_43[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_43[1], slope=Cfit_RothC_1_43[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_44[1], slope=Cfit_APSIM_1_44[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_44[1], slope=Cfit_Daycent_1_44[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_44[1], slope=Cfit_RothC_1_44[2],
  #             color=cbPalette9[3]) +
  scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_6_",clim_num,".jpg"),
         plot=gCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_6_",clim_num,".jpg"),
         plot=gCfut, width=9, height=6, dpi=300)


  #*************************************************************

# Daily graphs-combined scenarios -----------------------------------------

  
  ### Mean of all scenarios

  gAllNexp <- mean_daily_results[mean_daily_results$year>=experiment_start_year &
                                   mean_daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
# KBS   ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Future N2O Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllNexp

  gAllNfut <- mean_daily_results[mean_daily_results$year>=experiment_end_year &
                                   mean_daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
# KBS    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Future N2O Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_N2O_exp_",clim_num,".jpg"),
         plot=gAllNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_N2O_fut_",clim_num,".jpg"),
         plot=gAllNfut, width=9, height=6, dpi=300)


  gAllCHexp <- mean_daily_results[mean_daily_results$year>=experiment_start_year &
                                    mean_daily_results$climate_scenario_num==clim_num &
                                    mean_daily_results$mgmt_scenario_grp_num!=6,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(-85,0) +
    ylim(-175,0) +
    ggtitle(paste(site_name," Future CH4 Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCHexp

  gAllCHfut <- mean_daily_results[mean_daily_results$year>=experiment_end_year &
                                    mean_daily_results$climate_scenario_num==clim_num &
                                    mean_daily_results$mgmt_scenario_grp_num!=6,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(-85,0) +
    ylim(-175,0) +
    ggtitle(paste(site_name," Future CH4 Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_CH4_exp_",clim_num,".jpg"),
         plot=gAllCHexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_CH4_fut_",clim_num,".jpg"),
         plot=gAllCHfut, width=9, height=6, dpi=300)


  #*************************************************************

# Daily-scenario group 4 --------------------------------------------------


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==4 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==4 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_4_",clim_num,".jpg"),
         plot=gNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_4_",clim_num,".jpg"),
         plot=gNfut, width=9, height=6, dpi=300)


  # CH4

  gCHexp <- daily_results[daily_results$year>=experiment_start_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==4 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(-85, 0) +
    ylim(-175, 0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHexp

  gCHfut <- daily_results[daily_results$year>=experiment_end_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==4 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(-85,0) +
    ylim(-200,0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_exp_grp_4_",clim_num,".jpg"),
         plot=gCHexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_fut_grp_4_",clim_num,".jpg"),
         plot=gCHfut, width=9, height=6, dpi=300)


  #*************************************************************

# Daily-scenario group 5 --------------------------------------------------


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==5 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
#    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==5 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_5_",clim_num,".jpg"),
         plot=gNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_5_",clim_num,".jpg"),
         plot=gNfut, width=9, height=6, dpi=300)


  # CH4

  gCHexp <- daily_results[daily_results$year>=experiment_start_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==5 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(-85,0) +
    ylim(-200,0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHexp

  gCHfut <- daily_results[daily_results$year>=experiment_end_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==5 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(-85,0) +
    ylim(-175,0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_exp_grp_5_",clim_num,".jpg"),
         plot=gCHexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_fut_grp_5_",clim_num,".jpg"),
         plot=gCHfut, width=9, height=6, dpi=300)


  #*************************************************************

# Daily-scenario group 6 --------------------------------------------------


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM") &
                           daily_results$mgmt_scenario_grp_num==6 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM") &
                           daily_results$mgmt_scenario_grp_num==6 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
#KBS    ylim(0,100) +
    ylim(0,20) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_6_clim_",clim_num,".jpg"),
         plot=gNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_6_clim_",clim_num,".jpg"),
         plot=gNfut, width=9, height=6, dpi=300)


  #*************************************************************

# GWP bar charts ----------------------------------------------------------


y_breaks <- seq(-50, 150, by = 10)
#y_labels <- sprintf("%.1f", y_breaks)
#y_labels[c(2,4,6,8)] <- ""

  ## GWP model means with error bars
gGWPeb <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_GWP, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_GWP-sd_GWP, ymax=mean_GWP+sd_GWP),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 18.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste0(site_name," Global Warming Potential by ",
                end_fut_period_year,"-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
 # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPeb

## GWP model means with each individual model included
gGWPam <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=GWP,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_GWP), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 18.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  #KBS ylim(-35,60) +
#KBS  ylim(-155,90) +
  ylim(-15,20) +
  ggtitle(paste0(site_name," Global Warming Potential by ",
                 end_fut_period_year,"-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPam

gGWPmm <- gwp_means_piv[gwp_means_piv$source %in% c("mean_CO2e_SOC","mean_CO2e_N2O","mean_CO2e_CH4") &
                          gwp_means_piv$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=vals, fill=source)) +
  geom_col(position="stack") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 18.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(-20,20) +
  #ylim(-40,40) +
  ggtitle(paste0(site_name," Global Warming Potential by Source by ",
                end_fut_period_year,"-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Source") +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPmm

#####  Not working yet: split GWP bar graphs with error bars
#####  (see KBS_results/Archive/split_bar_graph_with_error_bars_example.R)
#
# gGWPmmeb <-
#   ggplot(gwp_means_piv[gwp_means_piv$source == "mean_CO2e_SOC" &
#                          gwp_means_piv$Climate_Scenario==clim_num,],
#          aes(x=scenario_abbrev, y=vals, fill=source)) +
#   geom_col(position="stack") +
#   geom_errorbar(data=gwp_means_piv[gwp_means_piv$source %in% c("mean_CO2e_SOC","sd_CO2c_SOC") &
#                                 gwp_means_piv$Climate_Scenario==clim_num,],
#                 aes(x=source,
#                     y=vals,
#                       ymin=gwp_means_piv[gwp_means_piv$source=="mean_CO2e_SOC","vals"]-
#                       ,
#                     ymax=gwp_means_piv[gwp_means_piv$source=="mean_CO2e_SOC","vals"]+
#                         gwp_means_piv[gwp_means_piv$source=="sd_CO2e_SOC","vals"]),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
#   xlab("") +
#   ggtitle(paste(site_name," Global Warming Potential by Source-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Source") +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
#
# gGWPmmeb

ggsave(filename=paste0(results_path,"pub_GWP_all_scenarios_errorbars_",clim_num,".jpg"),
       plot=gGWPeb, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_GWP_all_scenarios_allmodels",clim_num,".jpg"),
       plot=gGWPam, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_GWP_by_source_all_scenarios_",clim_num,".jpg"),
       plot=gGWPmm, width=9, height=6, dpi=300)

#*************************************************************

# Change over time charts -------------------------------------------------

#crop_ylim <- c(-2,1.5)
crop_ylim <- c(-0.5,1)

# ## Cotton with error bars
# 
# gMYchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
#   ggplot(aes(x=scenario_abbrev, y=mean_CottonYld_Mgha, fill=factor(scenario_abbrev))) +
#   geom_col(position="dodge") +
#   geom_errorbar(aes(ymin=mean_CottonYld_Mgha-sd_CottonYld_Mgha,
#                     ymax=mean_CottonYld_Mgha+sd_CottonYld_Mgha),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
#   xlab("") +
#   ylim(crop_ylim) +
#   ggtitle(paste(site_name,"Change in Cotton Yield by ",
#                 end_fut_period_year,"-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Scenario") +
#   # scale_y_continuous(breaks=y_breaks) +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# gMYchg

## Cotton all models
gCYchg_am <- summary_output[summary_output$Climate_Scenario==clim_num &
                              !is.na(summary_output$Cotton_Diff_Mgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=Cotton_Diff_Mgha,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CottonYld_Mgha), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 18.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Cotton_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(crop_ylim) +
  ggtitle(paste0(site_name," Change in Cotton Yield by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  #scale_y_continuous(breaks=crop_ylim) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gCYchg_am

# ## Sorghum with error bars
# 
# gSYchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
#   ggplot(aes(x=scenario_abbrev, y=mean_SorghumYld_Mgha, fill=factor(scenario_abbrev))) +
#   geom_col(position="dodge") +
#   geom_errorbar(aes(ymin=mean_SorghumYld_Mgha-sd_SorghumYld_Mgha,
#                     ymax=mean_SorghumYld_Mgha+sd_SorghumYld_Mgha),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
#   xlab("") +
#   ylim(crop_ylim) +
#   ggtitle(paste(site_name,"Change in Sorghum Yield by ",
#                 end_fut_period_year,"-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Scenario") +
#   # scale_y_continuous(breaks=y_breaks) +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# gSYchg

## Sorghum all models
gSYchg_am <- summary_output[summary_output$Climate_Scenario==clim_num &
                              !is.na(summary_output$Sorghum_Diff_Mgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=Sorghum_Diff_Mgha,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_SorghumYld_Mgha), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 18.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Sorghum_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(crop_ylim) +
  ggtitle(paste0(site_name," Change in Sorghum Yield by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  #scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gSYchg_am


# ## SOC with error bars
# 
# gSOCchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
#   ggplot(aes(x=scenario_abbrev, y=mean_SOC_Mgha, fill=factor(scenario_abbrev))) +
#   geom_col(position="dodge") +
#   geom_errorbar(aes(ymin=mean_SOC_Mgha-sd_SOC_Mgha,
#                     ymax=mean_SOC_Mgha+sd_SOC_Mgha),
#                 width=.2) +   # Width of the error bars
#   ylab(expression('SOC (Mg ha ' ^-1*')')) +
#   xlab("") +
# #  ylim(-20,60) +
#   ggtitle(paste(site_name,"Change in Soil Organic Carbon by ",
#                 end_fut_period_year,"-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Scenario") +
#   # scale_y_continuous(breaks=y_breaks) +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# gSOCchg

## GWP model means with each individual model included

## SOC
gSOCchg_am <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_SOC,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_SOC), color= "black", 
           fill=NA, position="dodge") +  
  annotate("rect", xmin = 0.5, xmax = 5.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T), 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 6.5, xmax = 7.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 7.5, xmax = 8.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 8.5, xmax = 12.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 12.5, xmax = 18.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  #ylim(-40,60) +
#  ylim(-155,80) +
  ylim(-25,30) +
  ggtitle(paste0(site_name," Change in CO2e-SOC by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gSOCchg_am

# ## N2O with error bars
# 
# gN2Ochg <- scenario_gas_means[scenario_gas_means$Climate_Scenario==clim_num,] %>%
#   ggplot(aes(x=scenario_abbrev, y=mean_N2O_Mgha, fill=factor(scenario_abbrev))) +
#   geom_col(position="dodge") +
#   geom_errorbar(aes(ymin=mean_N2O_Mgha-sd_N2O_Mgha,
#                     ymax=mean_N2O_Mgha+sd_N2O_Mgha),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   ylab(expression('N2O (Mg ha ' ^-1*')')) +
#   xlab("") +
# #  ylim(0,0.1) +
#   ggtitle(paste(site_name,"Change in Cumulative N2O Emissions by ",
#                 end_fut_period_year,"-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Scenario") +
#   # scale_y_continuous(breaks=y_breaks) +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# gN2Ochg

## N2O all models
gN2Ochg_am <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_N2O,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_N2O), color= "black", 
           fill=NA, position="dodge") +
  # geom_vline(xintercept=5.5, color = "grey") +
  # geom_vline(xintercept=6.5, color = "grey") +
  # geom_vline(xintercept=7.5, color = "grey") +
  # geom_vline(xintercept=8.5, color = "grey") +
  # geom_vline(xintercept=12.5, color = "grey") +
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0,
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 8.5, xmax = 12.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 12.5, xmax = 18.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  #ylim(0,9) +
#KBS  ylim(0,30) +
  ylim(0,5) +
  ggtitle(paste0(site_name," Change in CO2e-N2O by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gN2Ochg_am

# ## CH4 with error bars
# 
# gCH4chg <- scenario_gas_means[scenario_gas_means$Climate_Scenario==clim_num,] %>%
#   ggplot(aes(x=scenario_abbrev, y=mean_CH4_Mgha, fill=factor(scenario_abbrev))) +
#   geom_col(position="dodge") +
#   geom_errorbar(aes(ymin=mean_CH4_Mgha-sd_CH4_Mgha, 
#                     ymax=mean_CH4_Mgha+sd_CH4_Mgha),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   ylab(expression('CH4 (Mg ha ' ^-1*')')) +
#   xlab("") +
# #  ylim(-0.1,0) +
#   ggtitle(paste(site_name,"Change in Cumulative CH4 Emissions by ",
#                 end_fut_period_year,"-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Scenario") +
#   # scale_y_continuous(breaks=y_breaks) +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"), 
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# gCH4chg

## CH4 all models
gCH4chg_am <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_CH4,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T), 
           ymax = 0,
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 6.5, xmax = 7.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 7.5, xmax = 8.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0, 
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 8.5, xmax = 12.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0, 
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 12.5, xmax = 18.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0, 
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  #ylim(-3,0) +
#  ylim(-8,0) +
  ylim(-6,0) +
  ggtitle(paste0(site_name," Change in CH4 by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gCH4chg_am

## CH4 all models
gCH4chg_am2 <- summary_output[summary_output$Climate_Scenario==clim_num &
                               !is.na(summary_output$CH4_Diff_kgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_CH4,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 18.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste0(site_name," Change in CH4 by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gCH4chg_am2

# ggsave(filename=paste0(results_path,"pub_change_in_Cotton_all_scenarios_",clim_num,".jpg"),
#        plot=gMYchg, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_Cotton_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gCYchg_am, width=9, height=6, dpi=300)
# ggsave(filename=paste0(results_path,"pub_change_in_Sorghum_all_scenarios_",clim_num,".jpg"),
#        plot=gSYchg, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_Sorghum_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gSYchg_am, width=9, height=6, dpi=300)
# ggsave(filename=paste0(results_path,"pub_change_in_soc_all_scenarios_",clim_num,".jpg"),
#        plot=gSOCchg, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_soc_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gSOCchg_am, width=9, height=6, dpi=300)
# ggsave(filename=paste0(results_path,"pub_change_in_n2o_all_scenarios_",clim_num,".jpg"),
#        plot=gN2Ochg, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_n2o_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gN2Ochg_am, width=9, height=6, dpi=300)
# ggsave(filename=paste0(results_path,"pub_change_in_ch4_all_scenarios_",clim_num,".jpg"),
#        plot=gCH4chg, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_ch4_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gCH4chg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_ch4_all_scenarios_all_models2_",clim_num,".jpg"),
       plot=gCH4chg_am2, width=9, height=6, dpi=300)

} # end for loop through climate scenarios



# Combined climate scenarios ----------------------------------------------

## change over time graphs

## GWP graph

}) # end suppressMessages

