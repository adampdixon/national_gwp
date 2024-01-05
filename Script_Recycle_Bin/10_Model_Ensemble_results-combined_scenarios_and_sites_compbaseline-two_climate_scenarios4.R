#*************************************************************
# File: 10_Model_Ensemble_Results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios2
# Author: Ellen Maas
# Date: June 2022
# Description: Clones 10_Model_Ensemble_Results-combined_scenarios_and_sites_compbaseline
# and restricts results to just the baseline and high UKESM 
# future climate scenario.
#
# ****NOTE: This version calculates % change in model components
#            compared to baseline climate or management.
#*************************************************************
# Calls:
# f_model_coef
#*************************************************************
# Audit Log
# 9/15/2023 - cloned code.
#*************************************************************

suppressMessages({
  
  # start -------
  ## header and setup  ---------------
  
  print(paste0("Starting 10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios.R"))
  
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  library(grid)
  library(ggpubr)
  
  # rm(list=ls())
  # master_path <- "~/Modeling"
  # setwd(master_path)
  
  #create results folder if it doesn't already exist
  these_results_folder <- paste0("Comb_results_2scen_",end_fut_period_year,"/")
  if(!dir.exists(these_results_folder)) dir.create(these_results_folder)
  
  climate_factor_order <- c("Baseline","UKESM_High")
  
  #*************************************************************
  
  
  # Import component data --------------------------------------
  
  kbs_summary_output <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/summary_output_final.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(Climate_Scenario %in% c(1,5))
  kbs_scenario_means <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/scenario_means.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(Climate_Scenario %in% c(1,5)) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num"))  %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  kbs_annual_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/annual_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5))
  kbs_mean_annual_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                             "/mean_annual_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5))
  kbs_daily_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                       "/daily_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5))
  kbs_mean_daily_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                            "/mean_daily_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5))
  
  lrf_summary_output <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/summary_output_final.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(Climate_Scenario %in% c(1,5))
  lrf_scenario_means <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/scenario_means.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(Climate_Scenario %in% c(1,5)) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num"))  %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  lrf_annual_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/annual_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5))
  lrf_mean_annual_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                             "/mean_annual_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5))
  lrf_daily_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                       "/daily_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5))
  lrf_mean_daily_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                            "/mean_daily_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5))
  
  
  ## Rearrange data for output -----------------------------------------------
  
  
  kbs_summary_output_piv <- pivot_longer(kbs_summary_output,
                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  kbs_scenario_means_piv <- pivot_longer(kbs_scenario_means,
                                         c(-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name,
                                           -climate_desc),
                                         names_to="source",values_to="vals")
  kbs_annual_results_piv <- pivot_longer(kbs_annual_results,
                                         c(-year,-model_name,-scenario_name,
                                           -climate_scenario_num,
                                           -mgmt_scenario_grp_num,
                                           -mgmt_scenario_opt_num,
                                           -scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals")
  kbs_mean_annual_results_piv <- pivot_longer(kbs_mean_annual_results,
                                              c(-year,-scenario_name,
                                                -climate_scenario_num,
                                                -mgmt_scenario_grp_num,
                                                -mgmt_scenario_opt_num,
                                                -scenario_abbrev,-site_name),
                                              names_to="source",values_to="vals")
  kbs_daily_results_piv <- pivot_longer(kbs_daily_results,
                                        c(-date,-year,-dayofyear,-model_name,
                                          -climate_scenario_num,
                                          -mgmt_scenario_grp_num,
                                          -mgmt_scenario_opt_num,
                                          -scenario_name,
                                          -scenario_abbrev,-site_name),
                                        names_to="source",values_to="vals")
  kbs_mean_daily_results_piv <- pivot_longer(kbs_mean_daily_results,
                                             c(-date,-year,-dayofyear,
                                               -climate_scenario_num,
                                               -mgmt_scenario_grp_num,
                                               -mgmt_scenario_opt_num,
                                               -scenario_name,
                                               -scenario_abbrev,-site_name),
                                             names_to="source",values_to="vals")
  lrf_summary_output_piv <- pivot_longer(lrf_summary_output,
                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  lrf_scenario_means_piv <- pivot_longer(lrf_scenario_means,
                                         c(-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name,
                                           -climate_desc),
                                         names_to="source",values_to="vals")
  lrf_annual_results_piv <- pivot_longer(lrf_annual_results,
                                         c(-year,-model_name,-scenario_name,
                                           -climate_scenario_num,
                                           -mgmt_scenario_grp_num,
                                           -mgmt_scenario_opt_num,
                                           -scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals")
  lrf_mean_annual_results_piv <- pivot_longer(lrf_mean_annual_results,
                                              c(-year,-scenario_name,
                                                -climate_scenario_num,
                                                -mgmt_scenario_grp_num,
                                                -mgmt_scenario_opt_num,
                                                -scenario_abbrev,-site_name),
                                              names_to="source",values_to="vals")
  lrf_daily_results_piv <- pivot_longer(lrf_daily_results,
                                        c(-date,-year,-dayofyear,-model_name,
                                          -climate_scenario_num,
                                          -mgmt_scenario_grp_num,
                                          -mgmt_scenario_opt_num,
                                          -scenario_name,
                                          -scenario_abbrev,-site_name),
                                        names_to="source",values_to="vals")
  lrf_mean_daily_results_piv <- pivot_longer(lrf_mean_daily_results,
                                             c(-date,-year,-dayofyear,
                                               -climate_scenario_num,
                                               -mgmt_scenario_grp_num,
                                               -mgmt_scenario_opt_num,
                                               -scenario_name,
                                               -scenario_abbrev,-site_name),
                                             names_to="source",values_to="vals")
  
  ##
  
  daily_results <- rbind(kbs_daily_results,lrf_daily_results)
  
  mean_daily_results <- rbind(kbs_mean_daily_results,lrf_mean_daily_results)
  
  summary_output_piv <- rbind(kbs_summary_output_piv,lrf_summary_output_piv) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  scenario_means_piv <- rbind(kbs_scenario_means_piv,lrf_scenario_means_piv)
  
  gwp_scenario_means <- left_join(rbind(kbs_scenario_means[,c("mean_CO2e_N2O","mean_CO2e_CH4",
                                                              "mean_CO2e_SOC","mean_GWP",
                                                              "scenario_abbrev","site_name",
                                                              "Climate_Scenario")],
                                        lrf_scenario_means[,c("mean_CO2e_N2O","mean_CO2e_CH4",
                                                              "mean_CO2e_SOC","mean_GWP",
                                                              "scenario_abbrev","site_name",
                                                              "Climate_Scenario")]),
                                  unique(scenario_df[,c("climate_scenario_num",
                                                        "climate_desc")]),
                                  by=c("Climate_Scenario"="climate_scenario_num"))%>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  gwp_scenario_means_piv <- pivot_longer(gwp_scenario_means,c(-scenario_abbrev,
                                                              -site_name,
                                                              -Climate_Scenario,
                                                              -climate_desc),
                                         names_to="source",
                                         values_to="vals")
  
  gwp_summary_output <- left_join(rbind(kbs_summary_output[,c("Climate_Scenario","scenario_abbrev",
                                                              "Mgmt_Scenario","Scenario_Name",
                                                              "Model","site_name",
                                                              "CO2e_N2O","CO2e_CH4",
                                                              "CO2e_SOC","GWP")],
                                        lrf_summary_output[,c("Climate_Scenario","scenario_abbrev",
                                                              "Mgmt_Scenario","Scenario_Name",
                                                              "Model","site_name",
                                                              "CO2e_N2O","CO2e_CH4",
                                                              "CO2e_SOC","GWP")]),
                                  unique(scenario_df[,c("climate_scenario_num",
                                                        "climate_desc")]),
                                  by=c("Climate_Scenario"="climate_scenario_num"))%>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  gwp_summary_output_piv <- pivot_longer(gwp_summary_output,c(-scenario_abbrev,
                                                              -site_name,
                                                              -Climate_Scenario,
                                                              -climate_desc,
                                                              -Mgmt_Scenario,
                                                              -Scenario_Name,
                                                              -Model),
                                         names_to="source",
                                         values_to="vals")
  
  
  ### write data frame
  write.csv(gwp_scenario_means, file=paste0(these_results_folder,"/gwp_scenario_means.csv"),
            row.names=FALSE)
  write.csv(gwp_summary_output, file=paste0(these_results_folder,"/gwp_summary_output.csv"),
            row.names=FALSE)
  
  
  # Import model explanatory components data --------------------------------
  
  ## KBS ----------
  
  kbs_model_components <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                          "/Summary_future_output.csv")) %>%
    mutate(site_name="KBS",
           pub_climate_scenario=factor(case_when(Climate_Scenario==1 ~ "Baseline",
                                                 Climate_Scenario==2 ~ "GFDL_Low",
                                                 Climate_Scenario==3 ~ "GFDL_High",
                                                 Climate_Scenario==4 ~ "UKESM_Low",
                                                 Climate_Scenario==5 ~ "UKESM_High"),
                                       levels=climate_factor_order)
    )
  
  #****************
  #### temporary work-around until APSIM and Daycent model future results scripts
  #### are rerun. APSIM did an incorrect substraction to get SW_25cm_change.
  #### Daycent converted SW_25cm_first from a fraction to a fraction (assumed it was a
  #### percentage), which also resulted in SW_25cm_change being 100x too small.
  
    kbs_model_components[kbs_model_components$Model == "APSIM","SW_25cm_change"] <- 
      kbs_model_components[kbs_model_components$Model == "APSIM","SW_25cm_first"] -
      -1*(kbs_model_components[kbs_model_components$Model == "APSIM","SW_25cm_change"] +
                kbs_model_components[kbs_model_components$Model == "APSIM","SW_25cm_first"])
  kbs_model_components[kbs_model_components$Model == "Daycent","SW_25cm_first"] <- 
    kbs_model_components[kbs_model_components$Model == "Daycent","SW_25cm_first"] * 100
  kbs_model_components[kbs_model_components$Model == "Daycent","SW_25cm_change"] <- 
    kbs_model_components[kbs_model_components$Model == "Daycent","SW_25cm_change"] * 100
  #****************
  ## LRF ----------
  
  lrf_model_components <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                          "/Summary_future_output.csv")) %>%
    mutate(site_name="LRF",
           pub_climate_scenario=factor(case_when(Climate_Scenario==1 ~ "Baseline",
                                                 Climate_Scenario==2 ~ "GFDL_Low",
                                                 Climate_Scenario==3 ~ "GFDL_High",
                                                 Climate_Scenario==4 ~ "UKESM_Low",
                                                 Climate_Scenario==5 ~ "UKESM_High"),
                                       levels=climate_factor_order)
    )
  
  
  # GWP -------------
  
  ## stats ---------------------------------------------------------------
  
  ## select the management treatments with the least and greatest change
  ## over time of values of each gwp component (N2O, CH4, and SOC), and 
  ## include the difference between them. Grouped by site and climate
  ## scenario.
  
  ### N2O
  min_co2e_n2o <- gwp_scenario_means[,c(1,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_CO2e_N2O)
  
  max_co2e_n2o <- gwp_scenario_means[,c(1,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_CO2e_N2O)
  
  ### CH4
  min_co2e_ch4 <- gwp_scenario_means[,c(2,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_CO2e_CH4)
  
  max_co2e_ch4 <- gwp_scenario_means[,c(2,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_CO2e_CH4)
  
  ### SOC
  min_co2e_soc <- gwp_scenario_means[,c(3,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_CO2e_SOC)
  
  max_co2e_soc <- gwp_scenario_means[,c(3,5:8)]%>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_CO2e_SOC)
  
  ### GWP
  min_gwp <- gwp_scenario_means[,c(4:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_GWP)
  
  max_gwp <- gwp_scenario_means[,c(4:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_GWP)
  
  ### calculate diffs and combine into data frame
  
  summary_ghg_stats <- cbind(max_co2e_n2o$mean_CO2e_N2O-min_co2e_n2o$mean_CO2e_N2O,
                             min_co2e_n2o[,c("mean_CO2e_N2O","scenario_abbrev")],
                             max_co2e_n2o[,c("mean_CO2e_N2O","scenario_abbrev")],
                             max_co2e_ch4$mean_CO2e_CH4-min_co2e_ch4$mean_CO2e_CH4,
                             min_co2e_ch4[,c("mean_CO2e_CH4","scenario_abbrev")],
                             max_co2e_ch4[,c("mean_CO2e_CH4","scenario_abbrev")],
                             max_co2e_soc$mean_CO2e_SOC-min_co2e_soc$mean_CO2e_SOC,
                             min_co2e_soc[,c("mean_CO2e_SOC","scenario_abbrev")],
                             max_co2e_soc[,c("mean_CO2e_SOC","scenario_abbrev")],
                             max_gwp$mean_GWP-min_gwp$mean_GWP,
                             min_gwp[,c("mean_GWP","scenario_abbrev")],
                             max_gwp)
  colnames(summary_ghg_stats) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr","max_co2e_n2o","max_co2e_n2o_scen_abbr",
                                   "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr","max_co2e_ch4","max_co2e_ch4_scen_abbr",
                                   "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr","max_co2e_soc","max_co2e_soc_scen_abbr",
                                   "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr","max_co2e_gwp",
                                   "scenario_abbrev","site_name","Climate_Scenario","climate_desc")
  
  ### write data frame
  write.csv(summary_ghg_stats, file=paste0(these_results_folder,"/summary_ghg_stats_min_max.csv"),
            row.names=FALSE)
  
  
  #***********************************************
  
  ### calculate diffs between each treatment and the "control" at each site
  
  control_trts <- gwp_scenario_means[(gwp_scenario_means$scenario_abbrev=="CR" & gwp_scenario_means$site_name=="KBS") |
                                       (gwp_scenario_means$scenario_abbrev=="RR00-CR" & gwp_scenario_means$site_name=="LRF"),]
  
  diffs_from_controls <- merge(control_trts,gwp_scenario_means,
                               by=c("site_name","climate_desc")) %>%
    mutate(diff_co2e_n2o=mean_CO2e_N2O.x-mean_CO2e_N2O.y,
           diff_co2e_ch4=mean_CO2e_CH4.x-mean_CO2e_CH4.y,
           diff_co2e_soc=mean_CO2e_SOC.x-mean_CO2e_SOC.y,
           diff_gwp=mean_GWP.x-mean_GWP.y) %>%
    select(site_name,climate_desc,Climate_Scenario.y,scenario_abbrev.y,
           diff_co2e_n2o,diff_co2e_ch4,diff_co2e_soc,diff_gwp)
  
  ### write data frame
  write.csv(diffs_from_controls, file=paste0(these_results_folder,"/diffs_from_controls.csv"),
            row.names=FALSE)
  
  ## select the management treatments with the least and greatest differences
  ## from control treatments in each gwp component (N2O, CH4, and SOC), and 
  ## include the difference between them. Grouped by site and climate
  ## scenario.
  
  ### N2O
  min_diff_co2e_n2o <- diffs_from_controls[,c(1:5)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_co2e_n2o)
  
  max_diff_co2e_n2o <- diffs_from_controls[,c(1:5)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_co2e_n2o)
  
  ### CH4
  min_diff_co2e_ch4 <- diffs_from_controls[,c(1:4,6)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_co2e_ch4)
  
  max_diff_co2e_ch4 <- diffs_from_controls[,c(1:4,6)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_co2e_ch4)
  
  ### SOC
  min_diff_co2e_soc <- diffs_from_controls[,c(1:4,7)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_co2e_soc)
  
  max_diff_co2e_soc <- diffs_from_controls[,c(1:4,7)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_co2e_soc)
  
  ### GWP
  min_diff_gwp <- diffs_from_controls[,c(1:4,8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_gwp)
  
  max_diff_gwp <- diffs_from_controls[,c(1:4,8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_gwp)
  
  ### calculate diffs and combine into data frame
  
  summary_ghg_stats_diff_from_ctrl <- cbind(max_diff_co2e_n2o$diff_co2e_n2o-min_diff_co2e_n2o$diff_co2e_n2o,
                                            min_diff_co2e_n2o[,c("diff_co2e_n2o","scenario_abbrev.y")],
                                            max_diff_co2e_n2o[,c("diff_co2e_n2o","scenario_abbrev.y")],
                                            max_diff_co2e_ch4$diff_co2e_ch4-min_diff_co2e_ch4$diff_co2e_ch4,
                                            min_diff_co2e_ch4[,c("diff_co2e_ch4","scenario_abbrev.y")],
                                            max_diff_co2e_ch4[,c("diff_co2e_ch4","scenario_abbrev.y")],
                                            max_diff_co2e_soc$diff_co2e_soc-min_diff_co2e_soc$diff_co2e_soc,
                                            min_diff_co2e_soc[,c("diff_co2e_soc","scenario_abbrev.y")],
                                            max_diff_co2e_soc[,c("diff_co2e_soc","scenario_abbrev.y")],
                                            max_diff_gwp$diff_gwp-min_diff_gwp$diff_gwp,
                                            min_diff_gwp[,c("diff_gwp","scenario_abbrev.y")],
                                            max_diff_gwp[,c(5,2,4,1,3)])
  colnames(summary_ghg_stats_diff_from_ctrl) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr",
                                                  "max_co2e_n2o","max_co2e_n2o_scen_abbr",
                                                  "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr",
                                                  "max_co2e_ch4","max_co2e_ch4_scen_abbr",
                                                  "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr",
                                                  "max_co2e_soc","max_co2e_soc_scen_abbr",
                                                  "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr",
                                                  "max_co2e_gwp","max_co2e_gwp_scen_abbr",
                                                  "scenario_abbrev","site_name","Climate_Scenario")
  
  ### write data frame
  write.csv(summary_ghg_stats_diff_from_ctrl, 
            file=paste0(these_results_folder,"/summary_ghg_stats_diff_from_ctrl.csv"),
            row.names=FALSE)
  
  
  #***********************************************
  
  ### calculate diffs between each climate scenario for the same treatment at each site
  
  base_trts <- gwp_scenario_means[gwp_scenario_means$Climate_Scenario==1,]
  
  diffs_from_base <- merge(base_trts,gwp_scenario_means,
                           by=c("site_name","scenario_abbrev")) %>%
    mutate(diff_co2e_n2o=mean_CO2e_N2O.x-mean_CO2e_N2O.y,
           diff_co2e_ch4=mean_CO2e_CH4.x-mean_CO2e_CH4.y,
           diff_co2e_soc=mean_CO2e_SOC.x-mean_CO2e_SOC.y,
           diff_gwp=mean_GWP.x-mean_GWP.y) %>%
    select(site_name,climate_desc.y,Climate_Scenario.y,scenario_abbrev,
           diff_co2e_n2o,diff_co2e_ch4,diff_co2e_soc,diff_gwp)
  
  ### write data frame
  write.csv(diffs_from_base, 
            file=paste0(these_results_folder,"/diffs_from_base_climate.csv"),
            row.names=FALSE)
  
  ### select all management treatments for all climate scenarios, reporting
  ### the climate scenario with the least and greatest differences from
  ### the baseline climate for each management scenario.
  ### NOTE: for this script version of limiting future climate scenarios to just 
  ### the baseline and one CMIP6, this will only report the one CMIP6 model
  ### for all scenarios, so it's not of use for the point-scale study.
  
  ### N2O
  min_diff_co2e_n2o_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:5)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_co2e_n2o)
  
  max_diff_co2e_n2o_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:5)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_co2e_n2o)
  
  ### CH4
  min_diff_co2e_ch4_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,6)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_co2e_ch4)
  
  max_diff_co2e_ch4_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,6)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_co2e_ch4)
  
  # ##### add blank BC rows: no data because APSIM doesn't do methane
  # min_diff_co2e_ch4_clim <- min_diff_co2e_ch4_clim %>%
  #   rbind(data.frame(site_name=c("KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF"),
  #                    climate_desc.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  #                    Climate_Scenario.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  #                    scenario_abbrev=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
  #                                      "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
  #                    diff_co2e_ch4=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  #   )
  # 
  # max_diff_co2e_ch4_clim <- max_diff_co2e_ch4_clim %>%
  #   rbind(data.frame(site_name=c("KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF"),
  #                    climate_desc.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  #                    Climate_Scenario.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  #                    scenario_abbrev=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
  #                                      "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
  #                    diff_co2e_ch4=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  #   )
  
  ### SOC
  pre_min_diff_co2e_soc_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,7)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_co2e_soc) 
  
  pre_max_diff_co2e_soc_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,7)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_co2e_soc)
  
  # reduce duplicates to the minimum climate scenario
  min_diff_co2e_soc_clim <- pre_min_diff_co2e_soc_clim %>%
    group_by(site_name,scenario_abbrev) %>% 
    summarize(site_name=unique(site_name),
              Climate_Scenario.y=min(Climate_Scenario.y),
              diff_co2e_soc=unique(diff_co2e_soc)
    ) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num","climate_desc")]),
              by=c("Climate_Scenario.y"="climate_scenario_num")) 
  
  max_diff_co2e_soc_clim <- pre_max_diff_co2e_soc_clim %>%
    group_by(site_name,scenario_abbrev) %>% 
    summarize(site_name=unique(site_name),
              Climate_Scenario.y=min(Climate_Scenario.y),
              diff_co2e_soc=unique(diff_co2e_soc)
    ) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num","climate_desc")]),
              by=c("Climate_Scenario.y"="climate_scenario_num")) 
  
  ### GWP
  min_diff_gwp_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,8)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_gwp)
  
  max_diff_gwp_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,8)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_gwp)
  
  ### calculate diffs and combine into data frame
  
  summary_ghg_stats_diff_from_base_climate <- cbind(max_diff_co2e_n2o_clim$diff_co2e_n2o-min_diff_co2e_n2o_clim$diff_co2e_n2o,
                                                    min_diff_co2e_n2o_clim[,c("diff_co2e_n2o","climate_desc.y")],
                                                    max_diff_co2e_n2o_clim[,c("diff_co2e_n2o","climate_desc.y")],
                                                    max_diff_co2e_ch4_clim$diff_co2e_ch4-min_diff_co2e_ch4_clim$diff_co2e_ch4,
                                                    min_diff_co2e_ch4_clim[,c("diff_co2e_ch4","climate_desc.y")],
                                                    max_diff_co2e_ch4_clim[,c("diff_co2e_ch4","climate_desc.y")],
                                                    max_diff_co2e_soc_clim$diff_co2e_soc-min_diff_co2e_soc_clim$diff_co2e_soc,
                                                    min_diff_co2e_soc_clim[,c("diff_co2e_soc","climate_desc")],
                                                    max_diff_co2e_soc_clim[,c("diff_co2e_soc","climate_desc")],
                                                    max_diff_gwp_clim$diff_gwp-min_diff_gwp_clim$diff_gwp,
                                                    min_diff_gwp_clim[,c("diff_gwp","climate_desc.y")],
                                                    max_diff_gwp_clim[,c(5,2,4,1,3)])
  
  colnames(summary_ghg_stats_diff_from_base_climate) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr",
                                                          "max_co2e_n2o","max_co2e_n2o_climate_desc",
                                                          "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr",
                                                          "max_co2e_ch4","max_co2e_ch4_climate_desc",
                                                          "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr",
                                                          "max_co2e_soc","max_co2e_soc_climate_desc",
                                                          "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr",
                                                          "max_co2e_gwp","max_co2e_gwp_climate_desc",
                                                          "scenario_abbrev","site_name","Climate_Scenario")
  
  ### write data frame
  write.csv(summary_ghg_stats_diff_from_base_climate, 
            file=paste0(these_results_folder,"/summary_ghg_stats_diff_from_base_climate.csv"),
            row.names=FALSE)
  
  
  #***********************************************
  
  ### diffs across climate scenarios by model
  ### (to answer diff for biochar compared to everything else and diffs
  ### between models)
  
  base_trts_bymodel <- gwp_summary_output[gwp_summary_output$Climate_Scenario==1,]
  
  ukesm_trts_bymodel <- gwp_summary_output[gwp_summary_output$Climate_Scenario==5,]
  
  diffs_from_base_bymodel <- merge(base_trts_bymodel,ukesm_trts_bymodel,
                                   by=c("site_name","scenario_abbrev","Model")) %>%
    mutate(diff_co2e_n2o=CO2e_N2O.x-CO2e_N2O.y,
           diff_co2e_ch4=CO2e_CH4.x-CO2e_CH4.y,
           diff_co2e_soc=CO2e_SOC.x-CO2e_SOC.y,
           diff_gwp=GWP.x-GWP.y) %>%
    select(Model,site_name,climate_desc.y,Climate_Scenario.y,scenario_abbrev,
           diff_co2e_n2o,diff_co2e_ch4,diff_co2e_soc,diff_gwp)
  
  colnames(diffs_from_base_bymodel) <- c("Model","site_name","climate_desc","Climate_Scenario","scenario_abbrev",
                                         "diff_co2e_n2o", "diff_co2e_ch4","diff_co2e_soc","diff_co2e_gwp"
  )
  
  biochar_mean_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)=="B",],
                                                            !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                           ~ Model+site_name+climate_desc+Climate_Scenario,
                                           data=diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)=="B",],
                                           FUN=mean,
                                           na.action=na.pass) %>%
    mutate(group_ind="biochar_means")
  notbiochar_mean_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)!="B",],
                                                               !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                              ~ Model+site_name+climate_desc+Climate_Scenario,
                                              data=diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)!="B",],
                                              FUN=mean,
                                              na.action=na.pass) %>%
    mutate(group_ind="nonbiochar_means")
  
  allscenario_mean_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel,
                                                                !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                               ~ Model+site_name+climate_desc+Climate_Scenario,
                                               data=diffs_from_base_bymodel,
                                               FUN=mean,
                                               na.action=na.pass) %>%
    mutate(group_ind="allscenario_means")
  
  trtmt_mean_diffs_bymodel <- rbind(biochar_mean_diffs_bymodel,notbiochar_mean_diffs_bymodel,allscenario_mean_diffs_bymodel)
  
  ### now all mgmt scenarios individually
  allscenario_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel,
                                                           !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                          ~ Model+site_name+climate_desc+Climate_Scenario+scenario_abbrev,
                                          data=diffs_from_base_bymodel,
                                          FUN=mean,
                                          na.action=na.pass)
  
  
  ### write data frames
  write.csv(trtmt_mean_diffs_bymodel, 
            file=paste0(these_results_folder,"/biochar_notbiochar_mean_diffs_from_baseline_bymodel.csv"),
            row.names=FALSE)
  write.csv(allscenario_diffs_bymodel, 
            file=paste0(these_results_folder,"/allscenario_diffs_from_baseline_bymodel.csv"),
            row.names=FALSE)
  
  
  #*************************************************************
  
  ## bar charts ----------------------------------------------------------
  
  g_gwp <- summary_output_piv[summary_output_piv$source == "GWP",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_vline(xintercept=14.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_GWP), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Global Warming Potential by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                      values=cbPalette9[c(8,2,6,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_gwp
  
  ggsave(filename=paste0(these_results_folder,"/pub_all_GWP.jpg"),
         plot=g_gwp, width=16, height=10, dpi=300)
  
  ## components  ------------------------------------------------------------
  
  # g_n2oe <- summary_output_piv[summary_output_piv$source == "CO2e_N2O",] %>%
  #   ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
  #   scale_x_discrete() +
  #   geom_hline(yintercept=0, color="darkgrey") +
  #   geom_vline(xintercept=5.5, color="grey") +
  #   geom_vline(xintercept=7.5, color="grey") +
  #   geom_vline(xintercept=8.5, color="grey") +
  #   geom_vline(xintercept=10.5, color="grey") +
  #   geom_vline(xintercept=14.5, color="grey") +
  #   geom_vline(xintercept=20.5, color="grey") +
  #   geom_col(position="dodge",colour=NA) +
  #   geom_col(data=gwp_scenario_means,
  #            aes(x=scenario_abbrev, y=mean_CO2e_N2O), color= "black", 
  #            fill=NA, position="dodge") + 
  #   ylab(expression('CO'[2]*'e N'[2]*'O (Mg ha ' ^-1*')')) +
  #   xlab("") +
  #   ggtitle(paste0("Change in CO2e-N2O by ",
  #                  end_fut_period_year)) +
  #   scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
  #                     values=cbPalette9[c(8,2,6,3)],
  #                     name="Model") +
  #   facet_grid(climate_desc~site_name) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         panel.border = element_rect(colour = "darkgrey", fill=NA),
  #         strip.background = element_blank(),
  #         axis.line = element_line(),
  #         axis.text.x = element_text(angle = 45,
  #                                    hjust = 1),
  #         legend.position = "top",
  #         legend.key = element_blank())
  # 
  # g_n2oe
  
  g_n2oe <- summary_output_piv[summary_output_piv$source == "CO2e_N2O" &
                                 summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_vline(xintercept=14.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_N2O), color= "black", 
             fill=NA, position="dodge") + 
    #    ylab(expression('CO'[2]*'e N'[2]*'O (Mg ha ' ^-1*')')) +
    ylab(expression('N'[2]*'O'[CO2e]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    #    ggtitle(paste0("Change in CO2e-N2O by ",end_fut_period_year)) +
    ggtitle(bquote("Change in N"["2"]*"O"[CO2e]*" by"~.(end_fut_period_year))) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_n2oe
  
  
  # g_ch4e <- summary_output_piv[summary_output_piv$source == "CO2e_CH4",] %>%
  #   ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
  #   scale_x_discrete() +
  #   geom_hline(yintercept=0, color="darkgrey") +
  #   geom_vline(xintercept=5.5, color="grey") +
  #   geom_vline(xintercept=7.5, color="grey") +
  #   geom_vline(xintercept=8.5, color="grey") +
  #   geom_vline(xintercept=10.5, color="grey") +
  #   geom_vline(xintercept=14.5, color="grey") +
  #   geom_vline(xintercept=20.5, color="grey") +
  #   geom_col(position="dodge",colour=NA) +
  #   geom_col(data=gwp_scenario_means,
  #            aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black",
  #            fill=NA, position="dodge") +
  #   ylab(expression('CO'[2]*'e CH'[4]*' (Mg ha ' ^-1*')')) +
  #   xlab("") +
  #   ggtitle(paste0("Change in CO2e-CH4 by ",
  #                  end_fut_period_year)) +
  #   scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
  #                     values=cbPalette9[c(8,2,6,3)],
  #                     name="Model") +
  #   facet_grid(climate_desc~site_name) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         panel.border = element_rect(colour = "darkgrey", fill=NA),
  #         strip.background = element_blank(),
  #         axis.line = element_line(),
  #         axis.text.x = element_text(angle = 45,
  #                                    hjust = 1),
  #         legend.position = "top",
  #         legend.key = element_blank())
  # 
  # g_ch4e
  
  g_ch4e <- summary_output_piv[summary_output_piv$source == "CO2e_CH4" &
                                 summary_output_piv$Model == "Daycent",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_vline(xintercept=14.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('CH'[4][CO2e]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(bquote("Change in CH"[4][CO2e]*" by"~.(end_fut_period_year))) +
    scale_fill_manual(labels=c("Daycent"),
                      values=cbPalette9[c(2)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_ch4e
  
  g_soce <- summary_output_piv[summary_output_piv$source == "CO2e_SOC",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_vline(xintercept=14.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_SOC), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('SOC'[CO2e]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    # ggtitle(paste0("Change in CO2e-SOC by ",
    #                end_fut_period_year)) +
    ggtitle(bquote("Change in SOC"[CO2e]*" by"~.(end_fut_period_year))) +
    scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                      values=cbPalette9[c(8,2,6,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_soce
  
  ggsave(filename=paste0(these_results_folder,"/pub_all_n2oe.jpg"),
         plot=g_n2oe, width=16, height=10, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_all_ch4e.jpg"),
         plot=g_ch4e, width=16, height=10, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_all_soce.jpg"),
         plot=g_soce, width=16, height=10, dpi=300)
  
  #*******************************************************************
  
  ## by source -----------------------------------------------------------
  
  
  g_gwp_source <- gwp_scenario_means_piv[gwp_scenario_means_piv$source %in% 
                                           c("mean_CO2e_SOC","mean_CO2e_N2O","mean_CO2e_CH4"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=source)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_vline(xintercept=14.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="stack") +
    # annotate("rect", xmin = 0.5, xmax = 5.5,
    #          ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
    #          ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
    #          alpha = 0, color= "grey") +
    # annotate("rect", xmin = 5.5, xmax = 6.5,
    #          ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
    #          ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
    #          alpha = 0, color= "grey") +
    # annotate("rect", xmin = 6.5, xmax = 7.5,
    #          ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
    #          ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          alpha = 0, color= "grey") +
  # annotate("rect", xmin = 7.5, xmax = 8.5,
  #          ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          alpha = 0, color= "grey") +
  # annotate("rect", xmin = 8.5, xmax = 12.5,
  #          ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          alpha = 0, color= "grey") +
  # annotate("rect", xmin = 12.5, xmax = 18.5,
  #          ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
  #          alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Global Warming Potential by Source by ",end_fut_period_year)) +
    labs(fill = "source") +
    scale_fill_manual(labels=c(bquote("CH"[4][CO2e]),bquote("N"[2]*"O"[CO2e]),bquote("SOC"[CO2e])),
                      values=cbPalette12[c(10,11,12)],
                      name="Source") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_gwp_source
  
  ggsave(filename=paste0(these_results_folder,"/pub_all_GWP_by_source.jpg"),
         plot=g_gwp_source, width=16, height=10, dpi=300)
  
  #*******************************************************************
  
  # Crops ---------------------------------------------------------
  
  
  ## Bar charts --------------------------------------------------------------------
  
  g_maize <- kbs_summary_output_piv[kbs_summary_output_piv$source == "Maize_Diff_Mgha" &
                                      kbs_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=6.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=kbs_scenario_means,
             aes(x=scenario_abbrev, y=mean_MaizeYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("KBS Maize Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_maize
  
  
  g_soybean <- kbs_summary_output_piv[kbs_summary_output_piv$source == "Soybean_Diff_Mgha" &
                                        kbs_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=6.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=kbs_scenario_means,
             aes(x=scenario_abbrev, y=mean_SoyYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("KBS Soybean Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_soybean
  
  g_wheat <- kbs_summary_output_piv[kbs_summary_output_piv$source == "Wheat_Diff_Mgha" &
                                      kbs_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=6.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=kbs_scenario_means,
             aes(x=scenario_abbrev, y=mean_WheatYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("KBS Wheat Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_wheat
  
  g_cotton <- lrf_summary_output_piv[lrf_summary_output_piv$source == "Cotton_Diff_Mgha" &
                                       lrf_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=lrf_scenario_means,
             aes(x=scenario_abbrev, y=mean_CottonYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Lint Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("LRF Cotton Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_cotton
  
  g_sorghum <- lrf_summary_output_piv[lrf_summary_output_piv$source == "Sorghum_Diff_Mgha" &
                                        lrf_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=lrf_scenario_means,
             aes(x=scenario_abbrev, y=mean_SorghumYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("LRF Sorghum Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_sorghum
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_maize.jpg"),
         plot=g_maize, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_soybean.jpg"),
         plot=g_soybean, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_wheat.jpg"),
         plot=g_wheat, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_cotton.jpg"),
         plot=g_cotton, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_sorghum.jpg"),
         plot=g_sorghum, width=8, height=8, dpi=300)
  
  
  
  #*******************************************************************
  
  # Model components ---------------------------------------------------------
  
  ## over time in each scenario ------------------------------------
  
  ### KBS ---------------------
  
  kbs_model_component_apsim_base_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                  kbs_model_components$Model=="APSIM",] %>%
    # add % change in components over time
  mutate(SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
         SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
         SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
         DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
         DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
         DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
         DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
         SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
         DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
         SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
         SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
         SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
         SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
         NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
         NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
         NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
         NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
         N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
         N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
         N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
         N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
         N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
         BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
         BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
         HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
         HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
         CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
         CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
         CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
         SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
         DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
         DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
         DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
         WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
         WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
         WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
         WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
         WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
         WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
         SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
         SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
         SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
         SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
         NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
         NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
         NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
         CH4_pctchg = round(CH4_change/CH4_first*100,1),
         CI_pctchg = round(CI_change/CI_first*100,1)
  )
  
  
  kbs_model_component_daycent_base_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                  kbs_model_components$Model=="Daycent",] %>%
    # add % change in components over time
    mutate(SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  kbs_model_component_apsim_ukesm_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==5 &
                                                                  kbs_model_components$Model=="APSIM",] %>%
    # add % change in components over time
    mutate(SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  
  kbs_model_component_daycent_ukesm_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==5 &
                                                                    kbs_model_components$Model=="Daycent",] %>%
    # add % change in components over time
    mutate(SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )

  ### LRF ---------------------
  
  lrf_model_component_apsim_base_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                  lrf_model_components$Model=="APSIM",] %>%
    # add % change in components over time
    mutate(SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
           SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
           SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
           DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
           NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
           N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
           N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
           BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
           HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
           HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
           CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
           CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
           CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
           SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
           DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
           WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
           SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
           NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
 
  lrf_model_component_daycent_base_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                    lrf_model_components$Model=="Daycent",] %>%
    # add % change in components over time
    mutate(SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
           SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
           SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
           DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
           NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
           N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
           N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
           BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
           HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
           HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
           CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
           CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
           CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
           SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
           DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
           WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
           SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
           NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
    lrf_model_component_apsim_ukesm_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==5 &
                                                                     lrf_model_components$Model=="APSIM",] %>%
    # add % change in components over time
      mutate(SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
             SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
             SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
             SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
             DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
             DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
             DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
             DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
             DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
             SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
             DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
             SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
             SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
             SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
             SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
             SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
             NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
             NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
             NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
             NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
             NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
             N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
             N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
             N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
             N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
             N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
             N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
             BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
             BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
             HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
             HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
             CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
             CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
             CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
             SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
             DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
             DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
             DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
             DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
             DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
             WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
             WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
             WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
             WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
             WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
             WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
             WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
             SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
             SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
             SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
             SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
             NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
             NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
             NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
             NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
             NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
             CH4_pctchg = round(CH4_change/CH4_first*100,1),
             CI_pctchg = round(CI_change/CI_first*100,1)
      )
    
        
    lrf_model_component_daycent_ukesm_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==5 &
                                                                       lrf_model_components$Model=="Daycent",] %>%
    # add % change in components over time
          mutate(SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
                 SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
                 SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
                 SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
                 DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
                 DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
                 DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
                 DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
                 DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
                 SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
                 DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
                 SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
                 SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
                 SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
                 SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
                 SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
                 NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
                 NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
                 NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
                 NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
                 NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
                 N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
                 N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
                 N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
                 N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
                 N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
                 N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
                 BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
                 BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
                 HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
                 HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
                 CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
                 CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
                 CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
                 SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
                 DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
                 DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
                 DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
                 DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
                 DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
                 WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
                 WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
                 WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
                 WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
                 WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
                 WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
                 WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
                 SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
                 SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
                 SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
                 SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
                 NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
                 NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
                 NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
                 NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
                 NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
                 CH4_pctchg = round(CH4_change/CH4_first*100,1),
                 CI_pctchg = round(CI_change/CI_first*100,1)
          )
        
    
    ### rearrange dfs for graphing -------------------------
  
  kbs_model_component_apsim_base_time_piv <- pivot_longer(kbs_model_component_apsim_base_time,
                                                          c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                            -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                          names_to="source",values_to="vals")

  kbs_model_component_daycent_base_time_piv <- pivot_longer(kbs_model_component_daycent_base_time,
                                                          c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                            -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                          names_to="source",values_to="vals")

  kbs_model_component_apsim_ukesm_time_piv <- pivot_longer(kbs_model_component_apsim_ukesm_time,
                                                          c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                            -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                          names_to="source",values_to="vals")
  
  kbs_model_component_daycent_ukesm_time_piv <- pivot_longer(kbs_model_component_daycent_ukesm_time,
                                                            c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                              -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                            names_to="source",values_to="vals")
  
  lrf_model_component_apsim_base_time_piv <- pivot_longer(lrf_model_component_apsim_base_time,
                                                          c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                            -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                          names_to="source",values_to="vals")
  
  lrf_model_component_daycent_base_time_piv <- pivot_longer(lrf_model_component_daycent_base_time,
                                                            c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                              -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                            names_to="source",values_to="vals")
  
  lrf_model_component_apsim_ukesm_time_piv <- pivot_longer(lrf_model_component_apsim_ukesm_time,
                                                           c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                             -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                           names_to="source",values_to="vals")
  
  lrf_model_component_daycent_ukesm_time_piv <- pivot_longer(lrf_model_component_daycent_ukesm_time,
                                                             c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                                               -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                             names_to="source",values_to="vals")

    ## by all mgmt scenarios ------------------------------------------
  
  ## Calculate percent changes for each model component by model as compared
  ## separately to the baseline management scenarios. 
  
  ### KBS -----------------
  
  ## This will provide % change by model and management scenario, compared to the
  ## baseline management scenario, for each climate scenario separately
  
  #### NOTE: Warnings produced by this management grouping are due to an uneven number of scenarios
  ####       between APSIM and Daycent (biochar). The aggregate function produces unequal
  ####       object lengths when grouping APSIM vs. Daycent because of this, however, the 
  ####       resulting calculations are still correct.
  
  
  kbs_model_component_apsim_base_bymgmt <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                                             kbs_model_components$Model=="APSIM",], 
                                                                      !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                         Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
                                                     ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
                                                     data=kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                                 kbs_model_components$Model=="APSIM",], 
                                                     FUN=mean,
                                                     na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="CR"])/SW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="CR"])/SW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="CR"])/SW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="CR"])/DW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Scenario_Abbrev=="CR"])/DW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="CR"])/DW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="CR"])/DW_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="CR"])/SW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Scenario_Abbrev=="CR"])/DW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="CR"])/SoilT_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Scenario_Abbrev=="CR"])/SoilT_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="CR"])/SoilT_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Scenario_Abbrev=="CR"])/SoilT_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="CR"])/NO3_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Scenario_Abbrev=="CR"])/NO3_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="CR"])/NO3_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="CR"])/NO3_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Scenario_Abbrev=="CR"])/N2O_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Scenario_Abbrev=="CR"])/N2O_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="CR"])/N2O_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="CR"])/N2O_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="CR"])/N2O_profile_change[Scenario_Abbrev=="CR"]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Scenario_Abbrev=="CR"])/BC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Scenario_Abbrev=="CR"])/BN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Scenario_Abbrev=="CR"])/HC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Scenario_Abbrev=="CR"])/HN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Scenario_Abbrev=="CR"])/CinB_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Scenario_Abbrev=="CR"])/CinH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Scenario_Abbrev=="CR"])/CinBtoH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Scenario_Abbrev=="CR"])/SOC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="CR"])/DW_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="CR"])/DW_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="CR"])/DW_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="CR"])/WFPS_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="CR"])/SoilT_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="CR"])/SoilT_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="CR"])/SoilT_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="CR"])/SoilT_15cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="CR"])/NO3_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="CR"])/NO3_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="CR"])/NO3_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
           CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1)
    )
  
  kbs_model_component_daycent_base_bymgmt <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                                               kbs_model_components$Model=="Daycent",], 
                                                                        !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                           Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
                                                       ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
                                                       data=kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                                   kbs_model_components$Model=="Daycent",], 
                                                       FUN=mean,
                                                       na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="CR"])/SW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="CR"])/SW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="CR"])/SW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="CR"])/DW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Scenario_Abbrev=="CR"])/DW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="CR"])/DW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="CR"])/DW_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="CR"])/SW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Scenario_Abbrev=="CR"])/DW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="CR"])/SoilT_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Scenario_Abbrev=="CR"])/SoilT_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="CR"])/SoilT_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Scenario_Abbrev=="CR"])/SoilT_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="CR"])/NO3_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Scenario_Abbrev=="CR"])/NO3_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="CR"])/NO3_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="CR"])/NO3_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Scenario_Abbrev=="CR"])/N2O_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Scenario_Abbrev=="CR"])/N2O_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="CR"])/N2O_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="CR"])/N2O_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="CR"])/N2O_profile_change[Scenario_Abbrev=="CR"]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Scenario_Abbrev=="CR"])/BC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Scenario_Abbrev=="CR"])/BN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Scenario_Abbrev=="CR"])/HC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Scenario_Abbrev=="CR"])/HN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Scenario_Abbrev=="CR"])/CinB_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Scenario_Abbrev=="CR"])/CinH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Scenario_Abbrev=="CR"])/CinBtoH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Scenario_Abbrev=="CR"])/SOC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="CR"])/DW_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="CR"])/DW_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="CR"])/DW_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="CR"])/WFPS_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="CR"])/SoilT_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="CR"])/SoilT_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="CR"])/SoilT_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="CR"])/SoilT_15cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="CR"])/NO3_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="CR"])/NO3_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="CR"])/NO3_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
           CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1)
    )
  
  
  
  ## This will provide % change by model and climate scenario, compared to the
  ## baseline climate scenario, considering all management scenarios together
  kbs_model_component_means_byclimate <- aggregate(as.matrix(select(kbs_model_components, 
                                                                    !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                       Scenario_Name,Scenario_Abbrev,site_name)))
                                                   ~ Model+Climate_Scenario+site_name, 
                                                   data=kbs_model_components, 
                                                   FUN=mean,
                                                   na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  
  
  
  ### LRF -----------------
  
  ## This will provide % change by model and management scenario, compared to the
  ## baseline management scenario, considering all climate scenarios together
  
  #### NOTE: Warnings produced by the management grouping are due to an uneven number of scenarios
  ####       between APSIM and Daycent (biochar). The aggregate function produces unequal
  ####       object lengths when grouping APSIM vs. Daycent because of this, however, the 
  ####       resulting calculations are still correct.
  lrf_model_component_apsim_base_bymgmt <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                                             lrf_model_components$Model=="APSIM",], 
                                                                      !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                         Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
                                                     ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
                                                     data=lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                                 lrf_model_components$Model=="APSIM",], 
                                                     FUN=mean,
                                                     na.action=na.pass) %>%
    mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Scenario_Abbrev=="RR00-CR"])/SW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Scenario_Abbrev=="RR00-CR"])/SW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Scenario_Abbrev=="RR00-CR"])/SW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="RR00-CR"])/SW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="RR00-CR"])/DW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Scenario_Abbrev=="RR00-CR"])/DW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Scenario_Abbrev=="RR00-CR"])/DW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Scenario_Abbrev=="RR00-CR"])/SW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="RR00-CR"])/N2O_profile_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Scenario_Abbrev=="RR00-CR"])/BC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Scenario_Abbrev=="RR00-CR"])/BN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Scenario_Abbrev=="RR00-CR"])/HC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Scenario_Abbrev=="RR00-CR"])/HN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinB_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Scenario_Abbrev=="RR00-CR"])/SOC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="RR00-CR"])/DW_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="RR00-CR"])/DW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Scenario_Abbrev=="RR00-CR"])/DW_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Scenario_Abbrev=="RR00-CR"])/DW_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="RR00-CR"])/CH4_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="RR00-CR"])/CI_change[Scenario_Abbrev=="RR00-CR"]*100,1)
    )
  
  
  lrf_model_component_daycent_base_bymgmt <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                                               lrf_model_components$Model=="Daycent",], 
                                                                        !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                           Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
                                                       ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
                                                       data=lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                                   lrf_model_components$Model=="Daycent",], 
                                                       FUN=mean,
                                                       na.action=na.pass) %>%
    mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Scenario_Abbrev=="RR00-CR"])/SW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Scenario_Abbrev=="RR00-CR"])/SW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Scenario_Abbrev=="RR00-CR"])/SW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="RR00-CR"])/SW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="RR00-CR"])/DW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Scenario_Abbrev=="RR00-CR"])/DW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Scenario_Abbrev=="RR00-CR"])/DW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Scenario_Abbrev=="RR00-CR"])/SW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="RR00-CR"])/N2O_profile_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Scenario_Abbrev=="RR00-CR"])/BC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Scenario_Abbrev=="RR00-CR"])/BN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Scenario_Abbrev=="RR00-CR"])/HC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Scenario_Abbrev=="RR00-CR"])/HN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinB_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Scenario_Abbrev=="RR00-CR"])/SOC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="RR00-CR"])/DW_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="RR00-CR"])/DW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Scenario_Abbrev=="RR00-CR"])/DW_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Scenario_Abbrev=="RR00-CR"])/DW_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="RR00-CR"])/CH4_change[Scenario_Abbrev=="RR00-CR"]*100,1),
           CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="RR00-CR"])/CI_change[Scenario_Abbrev=="RR00-CR"]*100,1)
    )
  
  
  ## This will provide % change by model and climate scenario, compared to the
  ## baseline climate scenario, considering all management scenarios together
  lrf_model_component_means_byclimate <- aggregate(as.matrix(select(lrf_model_components, 
                                                                    !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                       Scenario_Name,Scenario_Abbrev,site_name)))
                                                   ~ Model+Climate_Scenario+site_name, 
                                                   data=lrf_model_components, 
                                                   FUN=mean,
                                                   na.action=na.pass) %>%
    mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Climate_Scenario==1])/SW_5cm_change[Climate_Scenario==1]*100,1),
           SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Climate_Scenario==1])/SW_15cm_change[Climate_Scenario==1]*100,1),
           SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Climate_Scenario==1])/SW_35cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Climate_Scenario==1])/DW_15cm_change[Climate_Scenario==1]*100,1),
           DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Climate_Scenario==1])/DW_35cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Climate_Scenario==1])/SW_10cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Climate_Scenario==1])/SoilT_35cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Climate_Scenario==1])/NO3_15cm_change[Climate_Scenario==1]*100,1),
           NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Climate_Scenario==1])/NO3_35cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Climate_Scenario==1])/N2O_5cm_change[Climate_Scenario==1]*100,1),
           N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Climate_Scenario==1])/N2O_15cm_change[Climate_Scenario==1]*100,1),
           N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Climate_Scenario==1])/N2O_35cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Climate_Scenario==1])/BC_10cm_change[Climate_Scenario==1]*100,1),
           BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Climate_Scenario==1])/BN_10cm_change[Climate_Scenario==1]*100,1),
           HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Climate_Scenario==1])/HC_10cm_change[Climate_Scenario==1]*100,1),
           HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Climate_Scenario==1])/HN_10cm_change[Climate_Scenario==1]*100,1),
           CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Climate_Scenario==1])/CinB_10cm_change[Climate_Scenario==1]*100,1),
           CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Climate_Scenario==1])/CinH_10cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Climate_Scenario==1])/CinBtoH_10cm_change[Climate_Scenario==1]*100,1),
           SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Climate_Scenario==1])/SOC_10cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Climate_Scenario==1])/DW_30cm_change[Climate_Scenario==1]*100,1),
           DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Climate_Scenario==1])/DW_45cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Climate_Scenario==1])/WFPS_30cm_change[Climate_Scenario==1]*100,1),
           WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Climate_Scenario==1])/WFPS_45cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Climate_Scenario==1])/SoilT_30cm_change[Climate_Scenario==1]*100,1),
           SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Climate_Scenario==1])/SoilT_45cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Climate_Scenario==1])/NO3_10cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Climate_Scenario==1])/NO3_30cm_change[Climate_Scenario==1]*100,1),
           NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Climate_Scenario==1])/NO3_45cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  
  
  
  ### rearrange dfs for graphing -------------------------
  
  
  
  
  
  kbs_model_component_means_apsim_climate <- kbs_model_component_means_byclimate[kbs_model_component_means_byclimate$Model=="APSIM",]
  
  
  lrf_model_component_means_apsim_climate <- lrf_model_component_means_byclimate[lrf_model_component_means_byclimate$Model=="APSIM",]
  
  
  kbs_model_component_means_daycent_climate <- kbs_model_component_means_byclimate[kbs_model_component_means_byclimate$Model=="Daycent",]
  
  
  lrf_model_component_means_daycent_climate <- lrf_model_component_means_byclimate[lrf_model_component_means_byclimate$Model=="Daycent",]
  
  
  
  #**********************************
  
  kbs_model_component_apsim_base_bymgmt_piv <- pivot_longer(kbs_model_component_apsim_base_bymgmt,
                                                            c(-Model,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                            names_to="source",values_to="vals")
  
  lrf_model_component_apsim_base_bymgmt_piv <- pivot_longer(lrf_model_component_apsim_base_bymgmt,
                                                            c(-Model,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                            names_to="source",values_to="vals")
  
  kbs_model_component_daycent_base_bymgmt_piv <- pivot_longer(kbs_model_component_daycent_base_bymgmt,
                                                              c(-Model,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                              names_to="source",values_to="vals")
  
  lrf_model_component_daycent_base_bymgmt_piv <- pivot_longer(lrf_model_component_daycent_base_bymgmt,
                                                              c(-Model,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
                                                              names_to="source",values_to="vals")
  
  
  
  kbs_model_component_means_apsim_climate_piv <- pivot_longer(kbs_model_component_means_apsim_climate,
                                                              c(-Model,-Climate_Scenario,-site_name),
                                                              names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","UKESM_High")))
  
  lrf_model_component_means_apsim_climate_piv <- pivot_longer(lrf_model_component_means_apsim_climate,
                                                              c(-Model,-Climate_Scenario,-site_name),
                                                              names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","UKESM_High")))
  
  
  kbs_model_component_means_daycent_climate_piv <- pivot_longer(kbs_model_component_means_daycent_climate,
                                                                c(-Model,-Climate_Scenario,-site_name),
                                                                names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
  lrf_model_component_means_daycent_climate_piv <- pivot_longer(lrf_model_component_means_daycent_climate,
                                                                c(-Model,-Climate_Scenario,-site_name),
                                                                names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
  
  
  
  ## by baseline climate and mgmt scenarios ------------------------------------------
  
  ## Now, calculate percent changes for each model component over all models
  ## for baseline climate and specific mgmt scenarios only.
  
  
  ### KBS ---------------------------------------------------------------------
  
  #### Group management by:
  #### * Biochar (BC)
  #### * Crop rotation (CR and with CC and NT)
  #### * Reduced fert (RF)
  #### * Residue removal (RR)
  
  kbs_model_component_means_byclimate_BCgrp <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Scenario_Abbrev %in% 
                                                                                                 c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),], 
                                                                          !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                             Scenario_Name,Scenario_Abbrev,pub_climate_scenario,
                                                                             site_name)))
                                                         ~ Model+Climate_Scenario+pub_climate_scenario+Scenario_Abbrev,
                                                         data=kbs_model_components[kbs_model_components$Scenario_Abbrev %in% 
                                                                                     c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),],
                                                         FUN=mean,
                                                         na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  
  
  kbs_model_component_means_byclimate_CR <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Scenario_Abbrev=="CR",], 
                                                                       !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                          Scenario_Name,Scenario_Abbrev,site_name)))
                                                      ~ Model+Climate_Scenario+Scenario_Abbrev,
                                                      data=kbs_model_components[kbs_model_components$Scenario_Abbrev=="CR",],
                                                      FUN=mean,
                                                      na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  kbs_model_component_means_byclimate_NTCR <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Scenario_Abbrev=="NT-CR",], 
                                                                         !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                            Scenario_Name,Scenario_Abbrev,site_name)))
                                                        ~ Model+Climate_Scenario,
                                                        data=kbs_model_components[kbs_model_components$Scenario_Abbrev=="NT-CR",],
                                                        FUN=mean,
                                                        na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  
  kbs_model_component_means_byclimate_CRgrp <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Scenario_Abbrev %in% c("CR","NT-CR"),], 
                                                                          !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                             Scenario_Name,Scenario_Abbrev,site_name)))
                                                         ~ Model+Climate_Scenario+Scenario_Abbrev,
                                                         data=kbs_model_components[kbs_model_components$Scenario_Abbrev %in% c("CR","NT-CR"),],
                                                         FUN=mean,
                                                         na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  kbs_model_component_means_byclimate_RFgrp <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Scenario_Abbrev %in% 
                                                                                                 c("RF05-CR","RF15-CR","RF25-CR","RF35-CR"),], 
                                                                          !c(Model,Climate_Scenario,Mgmt_Scenario,
                                                                             Scenario_Name,Scenario_Abbrev,pub_climate_scenario,
                                                                             site_name)))
                                                         ~ Model+Climate_Scenario+pub_climate_scenario+Scenario_Abbrev,
                                                         data=kbs_model_components[kbs_model_components$Scenario_Abbrev %in% 
                                                                                     c("RF05-CR","RF15-CR","RF25-CR","RF35-CR"),],
                                                         FUN=mean,
                                                         na.action=na.pass) %>%
    mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
           DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
           DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
           DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
           SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
           DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
           SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
           SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
           SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
           NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
           NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
           NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
           NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
           N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
           N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
           N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
           N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
           BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
           BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
           HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
           HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
           CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
           CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
           CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
           SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
           DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
           DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
           DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
           SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
           SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
           SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
           SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
           NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
           NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
           CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )  
  
  
  # ## by baseline climate and mgmt scenarios for each model ------------------------------------------
  # 
  # ## Now, calculate percent changes for each model component for
  # ## each model separately for baseline climate and choice mgmt 
  # ## scenarios only.
  # 
  # ###  APSIM
  # 
  # kbs_model_component_apsim_on_baseclimate_for_RR00CR <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
  #                                                                        SW_60cm_change,SW_25cm_change,
  #                                                                        DW_20cm_change,
  #                                                                        DW_40cm_change,DW_60cm_change,
  #                                                                        DW_0to60cm_change,DW_25cm_change,
  #                                                                        SoilT_20cm_change,SoilT_40cm_change,
  #                                                                        SoilT_60cm_change,SoilT_25cm_change,
  #                                                                        NO3_20cm_change,NO3_40cm_change,
  #                                                                        NO3_60cm_change,NO3_0to60cm_change,
  #                                                                        N2O_20cm_change,N2O_40cm_change,
  #                                                                        N2O_60cm_change,N2O_0to60cm_change,
  #                                                                        N2O_profile_change,
  #                                                                        BC_25cm_change,BN_25cm_change,
  #                                                                        HC_25cm_change,HN_25cm_change,
  #                                                                        CinB_25cm_change,CinH_25cm_change,
  #                                                                        CinBtoH_25cm_change,SOC_25cm_change) 
  #                                                                  ~ Model+Climate_Scenario+site_name, 
  #                                                                  data=kbs_model_components[kbs_model_components$Model=="APSIM" &
  #                                                                                              kbs_model_components$Scenario_Abbrev=="RR00-CR",], 
  #                                                                  FUN=mean,
  #                                                                  na.action=na.omit) %>%
  #   mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_40cm_pctchg = round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_25cm_pctchg = round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
  #          N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-SW_20cm_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
  #          BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
  #          HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
  #          HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
  #          CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
  #          CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
  #          SOC_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # lrf_model_component_apsim_on_baseclimate_for_RR00CR <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
  #                                                                        SW_60cm_change,SW_25cm_change,
  #                                                                        DW_20cm_change,
  #                                                                        DW_40cm_change,DW_60cm_change,
  #                                                                        DW_0to60cm_change,DW_25cm_change,
  #                                                                        SoilT_20cm_change,SoilT_40cm_change,
  #                                                                        SoilT_60cm_change,SoilT_25cm_change,
  #                                                                        NO3_20cm_change,NO3_40cm_change,
  #                                                                        NO3_60cm_change,NO3_0to60cm_change,
  #                                                                        N2O_20cm_change,N2O_40cm_change,
  #                                                                        N2O_60cm_change,N2O_0to60cm_change,
  #                                                                        N2O_profile_change,
  #                                                                        BC_25cm_change,BN_25cm_change,
  #                                                                        HC_25cm_change,HN_25cm_change,
  #                                                                        CinB_25cm_change,CinH_25cm_change,
  #                                                                        CinBtoH_25cm_change,SOC_25cm_change) 
  #                                                                  ~ Model+Climate_Scenario+site_name, 
  #                                                                  data=lrf_model_components[lrf_model_components$Model=="APSIM" &
  #                                                                                              lrf_model_components$Scenario_Abbrev=="RR00-CR",], 
  #                                                                  FUN=mean,
  #                                                                  na.action=na.omit) %>%
  #   mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_40cm_pctchg = round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_25cm_pctchg = round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
  #          N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-SW_20cm_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
  #          BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
  #          HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
  #          HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
  #          CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
  #          CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
  #          SOC_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # kbs_model_component_apsim_on_RR00CR_for_baseclimate <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
  #                                                                        SW_60cm_change,SW_25cm_change,
  #                                                                        DW_20cm_change,
  #                                                                        DW_40cm_change,DW_60cm_change,
  #                                                                        DW_0to60cm_change,DW_25cm_change,
  #                                                                        SoilT_20cm_change,SoilT_40cm_change,
  #                                                                        SoilT_60cm_change,SoilT_25cm_change,
  #                                                                        NO3_20cm_change,NO3_40cm_change,
  #                                                                        NO3_60cm_change,NO3_0to60cm_change,
  #                                                                        N2O_20cm_change,N2O_40cm_change,
  #                                                                        N2O_60cm_change,N2O_0to60cm_change,
  #                                                                        N2O_profile_change,
  #                                                                        BC_25cm_change,BN_25cm_change,
  #                                                                        HC_25cm_change,HN_25cm_change,
  #                                                                        CinB_25cm_change,CinH_25cm_change,
  #                                                                        CinBtoH_25cm_change,SOC_25cm_change) 
  #                                                                  ~ Model+Scenario_Abbrev+site_name, 
  #                                                                  data=kbs_model_components[kbs_model_components$Model=="APSIM" &
  #                                                                                              kbs_model_components$Climate_Scenario==1,], 
  #                                                                  FUN=mean,
  #                                                                  na.action=na.omit) %>%
  #   mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="RR00-CR"])/SW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="RR00-CR"])/SW_40cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="RR00-CR"])/SW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="RR00-CR"])/DW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Scenario_Abbrev=="RR00-CR"])/DW_40cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="RR00-CR"])/SW_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Scenario_Abbrev=="RR00-CR"])/DW_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_40cm_pctchg = round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="RR00-CR"])/SW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_60cm_pctchg = round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="RR00-CR"])/SW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_25cm_pctchg = round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="RR00-CR"])/SW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_40cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_40cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-SW_20cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_profile_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Scenario_Abbrev=="RR00-CR"])/BC_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Scenario_Abbrev=="RR00-CR"])/BN_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Scenario_Abbrev=="RR00-CR"])/HC_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Scenario_Abbrev=="RR00-CR"])/HN_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Scenario_Abbrev=="RR00-CR"])/CinB_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Scenario_Abbrev=="RR00-CR"])/CinH_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Scenario_Abbrev=="RR00-CR"])/CinBtoH_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SOC_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="RR00-CR"])/SW_25cm_change[Scenario_Abbrev=="RR00-CR"]*100,1)
  #   )
  # 
  # ## Daycent
  # 
  # kbs_model_component_daycent_on_baseclimate_for_CR <- aggregate(cbind(DW_20cm_change,DW_40cm_change,
  #                                                                      DW_60cm_change,DW_0to60cm_change,
  #                                                                      SW_25cm_change,DW_25cm_change,
  #                                                                      SoilT_20cm_change,SoilT_40cm_change,
  #                                                                      SoilT_60cm_change,SoilT_25cm_change,
  #                                                                      NO3_20cm_change,NO3_40cm_change,
  #                                                                      NO3_60cm_change,NO3_0to60cm_change,
  #                                                                      N2O_profile_change,SOC_25cm_change,
  #                                                                      DW_2cm_change,DW_5cm_change,
  #                                                                      DW_10cm_change,WFPS_2cm_change,
  #                                                                      WFPS_5cm_change,WFPS_10cm_change,
  #                                                                      WFPS_20cm_change,WFPS_40cm_change,
  #                                                                      WFPS_60cm_change,SoilT_2cm_change,
  #                                                                      SoilT_5cm_change,SoilT_10cm_change,
  #                                                                      SoilT_15cm_change,NO3_2cm_change,
  #                                                                      NO3_5cm_change,NO3_10cm_change,
  #                                                                      CH4_change,CI_change) 
  #                                                                ~ Model+Climate_Scenario+site_name, 
  #                                                                data=kbs_model_components[kbs_model_components$Model=="Daycent" &
  #                                                                                            kbs_model_components$Scenario_Abbrev=="CR",], 
  #                                                                FUN=mean,
  #                                                                na.action=na.omit) %>%
  #   mutate(DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # kbs_model_component_daycent_on_baseclimate_for_NTCR <- aggregate(cbind(DW_20cm_change,DW_40cm_change,
  #                                                                        DW_60cm_change,DW_0to60cm_change,
  #                                                                        SW_25cm_change,DW_25cm_change,
  #                                                                        SoilT_20cm_change,SoilT_40cm_change,
  #                                                                        SoilT_60cm_change,SoilT_25cm_change,
  #                                                                        NO3_20cm_change,NO3_40cm_change,
  #                                                                        NO3_60cm_change,NO3_0to60cm_change,
  #                                                                        N2O_profile_change,SOC_25cm_change,
  #                                                                        DW_2cm_change,DW_5cm_change,
  #                                                                        DW_10cm_change,WFPS_2cm_change,
  #                                                                        WFPS_5cm_change,WFPS_10cm_change,
  #                                                                        WFPS_20cm_change,WFPS_40cm_change,
  #                                                                        WFPS_60cm_change,SoilT_2cm_change,
  #                                                                        SoilT_5cm_change,SoilT_10cm_change,
  #                                                                        SoilT_15cm_change,NO3_2cm_change,
  #                                                                        NO3_5cm_change,NO3_10cm_change,
  #                                                                        CH4_change,CI_change) 
  #                                                                  ~ Model+Climate_Scenario+site_name, 
  #                                                                  data=kbs_model_components[kbs_model_components$Model=="Daycent" &
  #                                                                                              kbs_model_components$Scenario_Abbrev=="NT-CR",], 
  #                                                                  FUN=mean,
  #                                                                  na.action=na.omit) %>%
  #   mutate(DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # kbs_model_component_daycent_on_CR_for_baseclimate <- aggregate(cbind(DW_20cm_change,DW_40cm_change,
  #                                                                      DW_60cm_change,DW_0to60cm_change,
  #                                                                      SW_25cm_change,DW_25cm_change,
  #                                                                      SoilT_20cm_change,SoilT_40cm_change,
  #                                                                      SoilT_60cm_change,SoilT_25cm_change,
  #                                                                      NO3_20cm_change,NO3_40cm_change,
  #                                                                      NO3_60cm_change,NO3_0to60cm_change,
  #                                                                      N2O_profile_change,SOC_25cm_change,
  #                                                                      DW_2cm_change,DW_5cm_change,
  #                                                                      DW_10cm_change,WFPS_2cm_change,
  #                                                                      WFPS_5cm_change,WFPS_10cm_change,
  #                                                                      WFPS_20cm_change,WFPS_40cm_change,
  #                                                                      WFPS_60cm_change,SoilT_2cm_change,
  #                                                                      SoilT_5cm_change,SoilT_10cm_change,
  #                                                                      SoilT_15cm_change,NO3_2cm_change,
  #                                                                      NO3_5cm_change,NO3_10cm_change,
  #                                                                      CH4_change,CI_change) 
  #                                                                ~ Model+Scenario_Abbrev+site_name, 
  #                                                                data=kbs_model_components[kbs_model_components$Model=="Daycent" &
  #                                                                                            kbs_model_components$Climate_Scenario==1,], 
  #                                                                FUN=mean,
  #                                                                na.action=na.omit) %>%
  #   mutate(DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="CR"])/DW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Scenario_Abbrev=="CR"])/DW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="CR"])/DW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="CR"])/DW_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="CR"])/SW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Scenario_Abbrev=="CR"])/DW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="CR"])/SoilT_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Scenario_Abbrev=="CR"])/SoilT_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="CR"])/SoilT_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Scenario_Abbrev=="CR"])/SoilT_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="CR"])/NO3_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Scenario_Abbrev=="CR"])/NO3_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="CR"])/NO3_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="CR"])/NO3_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="CR"])/N2O_profile_change[Scenario_Abbrev=="CR"]*100,1),
  #          SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Scenario_Abbrev=="CR"])/SOC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="CR"])/DW_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="CR"])/DW_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="CR"])/DW_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="CR"])/WFPS_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="CR"])/SoilT_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="CR"])/SoilT_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="CR"])/SoilT_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="CR"])/SoilT_15cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="CR"])/NO3_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="CR"])/NO3_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1)
  #   )
  # 
  # ### LRF ---------------------------------------------------------------------
  # 
  # #### Group management by:
  # #### * Biochar (BC)
  # #### * Cover crops (CC and NT-CC)
  # #### * Reduced fert (RF)
  # #### * Residue removal (RR and NT-RR)
  # 
  # lrf_model_component_means_byclimate_BCgrp <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Scenario_Abbrev %in% 
  #                                                                                                c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),], 
  #                                                                         !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                            Scenario_Name,Scenario_Abbrev,pub_climate_scenario,
  #                                                                            site_name)))
  #                                                        ~ Model+Climate_Scenario+pub_climate_scenario+Scenario_Abbrev,
  #                                                        data=lrf_model_components[lrf_model_components$Scenario_Abbrev %in% 
  #                                                                                    c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),],
  #                                                        FUN=mean,
  #                                                        na.action=na.pass) %>%
  #   mutate(SW_5cm_pctchg_climate = round((SW_5cm_change-SW_5cm_change[Climate_Scenario==1])/SW_5cm_change[Climate_Scenario==1]*100,1),
  #          SW_15cm_pctchg_climate = round((SW_15cm_change-SW_15cm_change[Climate_Scenario==1])/SW_15cm_change[Climate_Scenario==1]*100,1),
  #          SW_35cm_pctchg_climate = round((SW_35cm_change-SW_35cm_change[Climate_Scenario==1])/SW_35cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg_climate = round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg_climate = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_15cm_pctchg_climate = round((DW_15cm_change-DW_15cm_change[Climate_Scenario==1])/DW_15cm_change[Climate_Scenario==1]*100,1),
  #          DW_35cm_pctchg_climate = round((DW_35cm_change-DW_35cm_change[Climate_Scenario==1])/DW_35cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg_climate = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg_climate = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_10cm_pctchg_climate = round((SW_10cm_change-SW_10cm_change[Climate_Scenario==1])/SW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg_climate = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg_climate = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg_climate = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_35cm_pctchg_climate = round((SoilT_35cm_change-SoilT_35cm_change[Climate_Scenario==1])/SoilT_35cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg_climate = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg_climate = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg_climate = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          NO3_15cm_pctchg_climate = round((NO3_15cm_change-NO3_15cm_change[Climate_Scenario==1])/NO3_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_35cm_pctchg_climate = round((NO3_35cm_change-NO3_35cm_change[Climate_Scenario==1])/NO3_35cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg_climate = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg_climate = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_5cm_pctchg_climate = round((N2O_5cm_change-N2O_5cm_change[Climate_Scenario==1])/N2O_5cm_change[Climate_Scenario==1]*100,1),
  #          N2O_15cm_pctchg_climate = round((N2O_15cm_change-N2O_15cm_change[Climate_Scenario==1])/N2O_15cm_change[Climate_Scenario==1]*100,1),
  #          N2O_35cm_pctchg_climate = round((N2O_35cm_change-N2O_35cm_change[Climate_Scenario==1])/N2O_35cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg_climate = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg_climate = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg_climate = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_10cm_pctchg_climate = round((BC_10cm_change-BC_10cm_change[Climate_Scenario==1])/BC_10cm_change[Climate_Scenario==1]*100,1),
  #          BN_10cm_pctchg_climate = round((BN_10cm_change-BN_10cm_change[Climate_Scenario==1])/BN_10cm_change[Climate_Scenario==1]*100,1),
  #          HC_10cm_pctchg_climate = round((HC_10cm_change-HC_10cm_change[Climate_Scenario==1])/HC_10cm_change[Climate_Scenario==1]*100,1),
  #          HN_10cm_pctchg_climate = round((HN_10cm_change-HN_10cm_change[Climate_Scenario==1])/HN_10cm_change[Climate_Scenario==1]*100,1),
  #          CinB_10cm_pctchg_climate = round((CinB_10cm_change-CinB_10cm_change[Climate_Scenario==1])/CinB_10cm_change[Climate_Scenario==1]*100,1),
  #          CinH_10cm_pctchg_climate = round((CinH_10cm_change-CinH_10cm_change[Climate_Scenario==1])/CinH_10cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_10cm_pctchg_climate = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Climate_Scenario==1])/CinBtoH_10cm_change[Climate_Scenario==1]*100,1),
  #          SOC_10cm_pctchg_climate = round((SOC_10cm_change-SOC_10cm_change[Climate_Scenario==1])/SOC_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg_climate = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg_climate = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg_climate = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_30cm_pctchg_climate = round((DW_30cm_change-DW_30cm_change[Climate_Scenario==1])/DW_30cm_change[Climate_Scenario==1]*100,1),
  #          DW_45cm_pctchg_climate = round((DW_45cm_change-DW_45cm_change[Climate_Scenario==1])/DW_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg_climate = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg_climate = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg_climate = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg_climate = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_30cm_pctchg_climate = round((WFPS_30cm_change-WFPS_30cm_change[Climate_Scenario==1])/WFPS_30cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_45cm_pctchg_climate = round((WFPS_45cm_change-WFPS_45cm_change[Climate_Scenario==1])/WFPS_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg_climate = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg_climate = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg_climate = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_30cm_pctchg_climate = round((SoilT_30cm_change-SoilT_30cm_change[Climate_Scenario==1])/SoilT_30cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_45cm_pctchg_climate = round((SoilT_45cm_change-SoilT_45cm_change[Climate_Scenario==1])/SoilT_45cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg_climate = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_10cm_pctchg_climate = round((NO3_10cm_change-NO3_10cm_change[Climate_Scenario==1])/NO3_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg_climate = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_30cm_pctchg_climate = round((NO3_30cm_change-NO3_30cm_change[Climate_Scenario==1])/NO3_30cm_change[Climate_Scenario==1]*100,1),
  #          NO3_45cm_pctchg_climate = round((NO3_45cm_change-NO3_45cm_change[Climate_Scenario==1])/NO3_45cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg_climate = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg_climate = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # 
  # lrf_model_component_means_byclimate_CCgrp <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Scenario_Abbrev %in% c("CC-CR","CC-NT-CR"),], 
  #                                                                         !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                            Scenario_Name,Scenario_Abbrev,site_name)))
  #                                                        ~ Model+Climate_Scenario+Scenario_Abbrev,
  #                                                        data=lrf_model_components[lrf_model_components$Scenario_Abbrev %in% c("CC-CR","CC-NT-CR"),],
  #                                                        FUN=mean,
  #                                                        na.action=na.pass) %>%
  #   mutate(SW_5cm_pctchg_climate = round((SW_5cm_change-SW_5cm_change[Climate_Scenario==1])/SW_5cm_change[Climate_Scenario==1]*100,1),
  #          SW_15cm_pctchg_climate = round((SW_15cm_change-SW_15cm_change[Climate_Scenario==1])/SW_15cm_change[Climate_Scenario==1]*100,1),
  #          SW_35cm_pctchg_climate = round((SW_35cm_change-SW_35cm_change[Climate_Scenario==1])/SW_35cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg_climate = round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg_climate = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_15cm_pctchg_climate = round((DW_15cm_change-DW_15cm_change[Climate_Scenario==1])/DW_15cm_change[Climate_Scenario==1]*100,1),
  #          DW_35cm_pctchg_climate = round((DW_35cm_change-DW_35cm_change[Climate_Scenario==1])/DW_35cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg_climate = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg_climate = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_10cm_pctchg_climate = round((SW_10cm_change-SW_10cm_change[Climate_Scenario==1])/SW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg_climate = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg_climate = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg_climate = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_35cm_pctchg_climate = round((SoilT_35cm_change-SoilT_35cm_change[Climate_Scenario==1])/SoilT_35cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg_climate = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg_climate = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg_climate = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          NO3_15cm_pctchg_climate = round((NO3_15cm_change-NO3_15cm_change[Climate_Scenario==1])/NO3_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_35cm_pctchg_climate = round((NO3_35cm_change-NO3_35cm_change[Climate_Scenario==1])/NO3_35cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg_climate = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg_climate = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_5cm_pctchg_climate = round((N2O_5cm_change-N2O_5cm_change[Climate_Scenario==1])/N2O_5cm_change[Climate_Scenario==1]*100,1),
  #          N2O_15cm_pctchg_climate = round((N2O_15cm_change-N2O_15cm_change[Climate_Scenario==1])/N2O_15cm_change[Climate_Scenario==1]*100,1),
  #          N2O_35cm_pctchg_climate = round((N2O_35cm_change-N2O_35cm_change[Climate_Scenario==1])/N2O_35cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg_climate = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg_climate = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg_climate = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_10cm_pctchg_climate = round((BC_10cm_change-BC_10cm_change[Climate_Scenario==1])/BC_10cm_change[Climate_Scenario==1]*100,1),
  #          BN_10cm_pctchg_climate = round((BN_10cm_change-BN_10cm_change[Climate_Scenario==1])/BN_10cm_change[Climate_Scenario==1]*100,1),
  #          HC_10cm_pctchg_climate = round((HC_10cm_change-HC_10cm_change[Climate_Scenario==1])/HC_10cm_change[Climate_Scenario==1]*100,1),
  #          HN_10cm_pctchg_climate = round((HN_10cm_change-HN_10cm_change[Climate_Scenario==1])/HN_10cm_change[Climate_Scenario==1]*100,1),
  #          CinB_10cm_pctchg_climate = round((CinB_10cm_change-CinB_10cm_change[Climate_Scenario==1])/CinB_10cm_change[Climate_Scenario==1]*100,1),
  #          CinH_10cm_pctchg_climate = round((CinH_10cm_change-CinH_10cm_change[Climate_Scenario==1])/CinH_10cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_10cm_pctchg_climate = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Climate_Scenario==1])/CinBtoH_10cm_change[Climate_Scenario==1]*100,1),
  #          SOC_10cm_pctchg_climate = round((SOC_10cm_change-SOC_10cm_change[Climate_Scenario==1])/SOC_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg_climate = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg_climate = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg_climate = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_30cm_pctchg_climate = round((DW_30cm_change-DW_30cm_change[Climate_Scenario==1])/DW_30cm_change[Climate_Scenario==1]*100,1),
  #          DW_45cm_pctchg_climate = round((DW_45cm_change-DW_45cm_change[Climate_Scenario==1])/DW_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg_climate = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg_climate = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg_climate = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg_climate = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_30cm_pctchg_climate = round((WFPS_30cm_change-WFPS_30cm_change[Climate_Scenario==1])/WFPS_30cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_45cm_pctchg_climate = round((WFPS_45cm_change-WFPS_45cm_change[Climate_Scenario==1])/WFPS_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg_climate = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg_climate = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg_climate = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_30cm_pctchg_climate = round((SoilT_30cm_change-SoilT_30cm_change[Climate_Scenario==1])/SoilT_30cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_45cm_pctchg_climate = round((SoilT_45cm_change-SoilT_45cm_change[Climate_Scenario==1])/SoilT_45cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg_climate = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_10cm_pctchg_climate = round((NO3_10cm_change-NO3_10cm_change[Climate_Scenario==1])/NO3_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg_climate = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_30cm_pctchg_climate = round((NO3_30cm_change-NO3_30cm_change[Climate_Scenario==1])/NO3_30cm_change[Climate_Scenario==1]*100,1),
  #          NO3_45cm_pctchg_climate = round((NO3_45cm_change-NO3_45cm_change[Climate_Scenario==1])/NO3_45cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg_climate = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg_climate = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # lrf_model_component_means_byclimate_RFgrp <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Scenario_Abbrev %in% 
  #                                                                                                c("RF05-CR","RF15-CR","RF25-CR","RF35-CR"),], 
  #                                                                         !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                            Scenario_Name,Scenario_Abbrev,pub_climate_scenario,
  #                                                                            site_name)))
  #                                                        ~ Model+Climate_Scenario+pub_climate_scenario+Scenario_Abbrev,
  #                                                        data=lrf_model_components[lrf_model_components$Scenario_Abbrev %in% 
  #                                                                                    c("RF05-CR","RF15-CR","RF25-CR","RF35-CR"),],
  #                                                        FUN=mean,
  #                                                        na.action=na.pass) %>%
  #   mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Climate_Scenario==1])/SW_5cm_change[Climate_Scenario==1]*100,1),
  #          SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Climate_Scenario==1])/SW_15cm_change[Climate_Scenario==1]*100,1),
  #          SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Climate_Scenario==1])/SW_35cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Climate_Scenario==1])/DW_15cm_change[Climate_Scenario==1]*100,1),
  #          DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Climate_Scenario==1])/DW_35cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Climate_Scenario==1])/SW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Climate_Scenario==1])/SoilT_35cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Climate_Scenario==1])/NO3_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Climate_Scenario==1])/NO3_35cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Climate_Scenario==1])/N2O_5cm_change[Climate_Scenario==1]*100,1),
  #          N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Climate_Scenario==1])/N2O_15cm_change[Climate_Scenario==1]*100,1),
  #          N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Climate_Scenario==1])/N2O_35cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Climate_Scenario==1])/BC_10cm_change[Climate_Scenario==1]*100,1),
  #          BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Climate_Scenario==1])/BN_10cm_change[Climate_Scenario==1]*100,1),
  #          HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Climate_Scenario==1])/HC_10cm_change[Climate_Scenario==1]*100,1),
  #          HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Climate_Scenario==1])/HN_10cm_change[Climate_Scenario==1]*100,1),
  #          CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Climate_Scenario==1])/CinB_10cm_change[Climate_Scenario==1]*100,1),
  #          CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Climate_Scenario==1])/CinH_10cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Climate_Scenario==1])/CinBtoH_10cm_change[Climate_Scenario==1]*100,1),
  #          SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Climate_Scenario==1])/SOC_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Climate_Scenario==1])/DW_30cm_change[Climate_Scenario==1]*100,1),
  #          DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Climate_Scenario==1])/DW_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Climate_Scenario==1])/WFPS_30cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Climate_Scenario==1])/WFPS_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Climate_Scenario==1])/SoilT_30cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Climate_Scenario==1])/SoilT_45cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Climate_Scenario==1])/NO3_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Climate_Scenario==1])/NO3_30cm_change[Climate_Scenario==1]*100,1),
  #          NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Climate_Scenario==1])/NO3_45cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # ### rearrange dfs for graphing ----------------------------------------------
  # 
  # kbs_model_component_means_byclimate_RFgrp_piv <- pivot_longer(kbs_model_component_means_byclimate_RFgrp,
  #                                                               c(-Model,-Climate_Scenario,-pub_climate_scenario,
  #                                                                 -Scenario_Abbrev),
  #                                                               names_to="source",values_to="vals") %>%
  #   left_join(unique(scenario_df[,c("climate_scenario_num",
  #                                   "climate_desc")]),
  #             by=c("Climate_Scenario"="climate_scenario_num")) %>%
  #   mutate(climate_desc = factor(climate_desc,
  #                                levels=climate_factor_order))
  # 
  # lrf_model_component_means_byclimate_RFgrp_piv <- pivot_longer(lrf_model_component_means_byclimate_RFgrp,
  #                                                               c(-Model,-Climate_Scenario,-pub_climate_scenario,
  #                                                                 -Scenario_Abbrev),
  #                                                               names_to="source",values_to="vals") %>%
  #   left_join(unique(scenario_df[,c("climate_scenario_num",
  #                                   "climate_desc")]),
  #             by=c("Climate_Scenario"="climate_scenario_num")) %>%
  #   mutate(climate_desc = factor(climate_desc,
  #                                levels=climate_factor_order))
  # 
  # 
  # model_component_apsim_climate_RR00CR <- rbind(kbs_model_component_apsim_on_baseclimate_for_RR00CR)
  # 
  # model_component_apsim_mgmt_baseline <- rbind(kbs_model_component_apsim_on_RR00CR_for_baseclimate)
  # 
  # model_component_means_daycent_climate_CR <- rbind(kbs_model_component_daycent_on_baseclimate_for_CR)
  # 
  # model_component_means_daycent_mgmt_baseline <- rbind(kbs_model_component_daycent_on_CR_for_baseclimate)
  # 
  # 
  # kbs_model_component_means_byclimate_CR_piv <- pivot_longer(kbs_model_component_means_byclimate_CR,
  #                                                            c(-Model,-Climate_Scenario,-Scenario_Abbrev),
  #                                                            names_to="source",values_to="vals") %>%
  #   left_join(unique(scenario_df[,c("climate_scenario_num",
  #                                   "climate_desc")]),
  #             by=c("Climate_Scenario"="climate_scenario_num")) %>%
  #   mutate(climate_desc = factor(climate_desc,
  #                                levels=climate_factor_order))
  # 
  # model_component_means_apsim_mgmt_baseline_piv <- pivot_longer(model_component_means_apsim_mgmt_baseline,
  #                                                               c(-Model,-Scenario_Abbrev,-site_name),
  #                                                               names_to="source",values_to="vals")
  # 
  # model_component_means_daycent_climate_CR_piv <- pivot_longer(model_component_means_daycent_climate_CR,
  #                                                              c(-Model,-Climate_Scenario,-site_name),
  #                                                              names_to="source",values_to="vals") %>%
  #   left_join(unique(scenario_df[,c("climate_scenario_num",
  #                                   "climate_desc")]),
  #             by=c("Climate_Scenario"="climate_scenario_num")) %>%
  #   mutate(climate_desc = factor(climate_desc,
  #                                levels=climate_factor_order))
  # 
  # model_component_means_daycent_mgmt_baseline_piv <- pivot_longer(model_component_means_daycent_mgmt_baseline,
  #                                                                 c(-Model,-Scenario_Abbrev,-site_name),
  #                                                                 names_to="source",values_to="vals") 
  # 
  
  # Calculate summary stats -------------------------------------------
  
  ## summarize soil temperature differences between sites --------------------
  ### Note: need to have run 0_Observations_and_constants_[site].R for a site first
  #### this will average soil temps on all 3 treatments used in this study
  #mean_soilt <- mean(pull(ObsTemp_all[ObsTemp_all$treatment %in% c("T1","T2","T3"),"soil_temperature"]))
  #KBS=12.427
  #mean_soilt <- mean(pull(ObsTemp_all[,"soil_temperature"]),na.rm=TRUE)
  #LRF=18.537
  #obs_diff_soilt_between_sites <- 18.537-12.427
  #diff=6.11
  #mean_soilm <- mean(pull(ObsVSM_mean[ObsVSM_mean$treatment %in% c("T1","T2","T3"),"mean_VSM"]),na.rm=TRUE)
  #KBS=0.211
  #mean_soilm <- mean(DayM_V$layer3,na.rm=TRUE)
  #LRF=0.114
  #diff_soilm_between_sites <- 0.211 - 0.114
  #diff=0.10
  
  # Graph -----------------
  
  ## over time (base) for each management scenario ------------------------------
  #### percent change in means of APSIM model components over time
  #### through the end of the future period in baseline climate
  
  n2o_font_size <- 50
  
  #### KBS-N2O --------------------------------------
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  
  g_kbs_apsim_base_n2o_time_n2o_20cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("N2O_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-20 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_time_n2o_20cm
  
  #### N2O - NO3
  g_kbs_apsim_base_n2o_time_no3_20cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("NO3_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-20 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_time_no3_20cm
  
  
  #### N2O - SoilT
  g_kbs_apsim_base_n2o_time_soilt_20cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SoilT_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-20 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_time_soilt_20cm
  
  #### N2O - SW
  g_kbs_apsim_base_n2o_time_soilw_20cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SW_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-20 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_time_soilw_20cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_kbs_daycent_base_n2o_time_n2o_profile <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("N2O_profile_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_n2o_time_n2o_profile
  
  
  #### N2O - NO3
  g_kbs_daycent_base_n2o_time_no3_10cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("NO3_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-10 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_n2o_time_no3_10cm
  
  #### N2O - SoilTemp not used by Daycent
  
  #### N2O - WFPS
  g_kbs_daycent_base_n2o_time_wfps_10cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_n2o_time_wfps_10cm
  
  #### group all N2O plots into one figure
  
  gg_kbs_n2o_time_base <- ggarrange(
    g_kbs_apsim_base_n2o_time_n2o_20cm,
    g_kbs_daycent_base_n2o_time_n2o_profile,
    g_kbs_apsim_base_n2o_time_no3_20cm,
    g_kbs_daycent_base_n2o_time_no3_10cm,
    g_kbs_apsim_base_n2o_time_soilt_20cm,
    NULL,
    g_kbs_apsim_base_n2o_time_soilw_20cm,
    g_kbs_daycent_base_n2o_time_wfps_10cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_kbs_n2o_time_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_n2o_expl_time_base.jpg"),
         plot=gg_kbs_n2o_time_base, width=30, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### KBS-CH4 --------------------------------------
  
  ch4_font_size <- 38
  
  ##### Daycent --------------------------------------
  
  #### CH4 - CH4
  g_kbs_daycent_base_ch4_time_ch4_15cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("CH4_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CH"["4"]*" 0-15 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_ch4_15cm
  
  #### CH4 - SoilTemp 2cm
  g_kbs_daycent_base_ch4_time_soilt_2cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SoilT_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_soilt_2cm
  
  #### CH4 - SoilTemp 5cm
  g_kbs_daycent_base_ch4_time_soilt_5cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SoilT_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_soilt_5cm
  
  #### CH4 - SoilTemp 10cm
  g_kbs_daycent_base_ch4_time_soilt_10cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SoilT_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_soilt_10cm

  #### CH4 - SoilTemp 20cm
  g_kbs_daycent_base_ch4_time_soilt_20cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SoilT_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_soilt_20cm
  
  #### CH4 - WFPS 2cm
  g_kbs_daycent_base_ch4_time_wfps_2cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("WFPS_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_wfps_2cm
  
  #### CH4 - WFPS 5cm
  g_kbs_daycent_base_ch4_time_wfps_5cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("WFPS_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_wfps_5cm
  
  #### CH4 - WFPS 10cm
  g_kbs_daycent_base_ch4_time_wfps_10cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_wfps_10cm
  
  #### CH4 - WFPS 20cm
  g_kbs_daycent_base_ch4_time_wfps_20cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("WFPS_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_time_wfps_20cm
  
  #### group all CH4 plots into one figure
  
  gg_kbs_ch4_time_base <- ggarrange(
    g_kbs_daycent_base_ch4_time_ch4_15cm,
    g_kbs_daycent_base_ch4_time_soilt_2cm,
    g_kbs_daycent_base_ch4_time_soilt_5cm,
    g_kbs_daycent_base_ch4_time_soilt_10cm,
    g_kbs_daycent_base_ch4_time_soilt_20cm,
    g_kbs_daycent_base_ch4_time_wfps_2cm,
    g_kbs_daycent_base_ch4_time_wfps_5cm,
    g_kbs_daycent_base_ch4_time_wfps_10cm,
    g_kbs_daycent_base_ch4_time_wfps_20cm,
    ncol=1,
    nrow=9,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = ch4_font_size+10, family = "serif"),
                    top = text_grob("Daycent",
                                    size = ch4_font_size+20, family="serif"))
  
  gg_kbs_ch4_time_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_ch4_expl_time_base.jpg"),
         plot=gg_kbs_ch4_time_base, width=22, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### KBS-SOC (Biochar) --------------------------------------
  
  biocharsoc_font_size <- 40
  # legend_theme <- theme(legend.text = element_text(size = 28), 
  #                       legend.title = element_text(size = 30), 
  #                       legend.key.size = unit(1, 'cm'))
  legend_theme <- theme(legend.text = element_blank(), 
                        legend.title = element_blank
                        )
  
  ##### APSIM --------------------------------------
  
  #### SOC - SOC
  
  g_kbs_apsim_base_biocharsoc_time_soc_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SOC_25cm_pctchg") &
      kbs_model_component_apsim_base_time_piv$Scenario_Abbrev %in% 
      c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank()) +
    legend_theme
  
  g_kbs_apsim_base_biocharsoc_time_soc_25cm
  
  #### SOC - CinputBio
  
  g_kbs_apsim_base_biocharsoc_time_cinb_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("CinB_25cm_pctchg") &
      kbs_model_component_apsim_base_time_piv$Scenario_Abbrev %in% 
      c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinB 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank()) +
    legend_theme
  
  g_kbs_apsim_base_biocharsoc_time_cinb_25cm
  
  
  #### SOC - CinputHum
  
  g_kbs_apsim_base_biocharsoc_time_cinh_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("CinH_25cm_pctchg") &
      kbs_model_component_apsim_base_time_piv$Scenario_Abbrev %in% 
      c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinH 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank()) +
    legend_theme
  
  g_kbs_apsim_base_biocharsoc_time_cinh_25cm
  
  #### SOC - CinputBtoH
  
  g_kbs_apsim_base_biocharsoc_time_cinbh_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("CinBtoH_25cm_pctchg") &
      kbs_model_component_apsim_base_time_piv$Scenario_Abbrev %in% 
      c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinBtoH 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank()) +
    legend_theme
  
  g_kbs_apsim_base_biocharsoc_time_cinbh_25cm
  
  #### SOC - SoilT
  g_kbs_apsim_base_biocharsoc_time_soilt_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SoilT_25cm_pctchg") &
      kbs_model_component_apsim_base_time_piv$Scenario_Abbrev %in% 
      c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank()) +
    legend_theme
  
  g_kbs_apsim_base_biocharsoc_time_soilt_25cm
  
  #### SOC - SW
  g_kbs_apsim_base_biocharsoc_time_soilw_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SW_25cm_pctchg") &
      kbs_model_component_apsim_base_time_piv$Scenario_Abbrev %in% 
      c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank()) +
    legend_theme
  
  g_kbs_apsim_base_biocharsoc_time_soilw_25cm
  
  #### group all N2O plots into one figure
  
  gg_kbs_biocharsoc_time_base <- ggarrange(
    g_kbs_apsim_base_biocharsoc_time_soc_25cm,
    g_kbs_apsim_base_biocharsoc_time_cinb_25cm,
    g_kbs_apsim_base_biocharsoc_time_cinh_25cm,
    g_kbs_apsim_base_biocharsoc_time_cinbh_25cm,
    g_kbs_apsim_base_biocharsoc_time_soilt_25cm,
    g_kbs_apsim_base_biocharsoc_time_soilw_25cm,
    ncol=1,
    nrow=6,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=FALSE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("KBS (baseline)",
                                    size = n2o_font_size+10, family="serif"))
  
  gg_kbs_biocharsoc_time_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_biocharsoc_expl_time_base.jpg"),
         plot=gg_kbs_biocharsoc_time_base, width=15, height=40, dpi=300)
  
  #### KBS-SOC  --------------------------------------
  
  soc_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### SOC - SOC
  
  g_kbs_apsim_base_soc_time_soc_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SOC_25cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_time_soc_25cm
  
  #### SOC - CinputBio
  
  g_kbs_apsim_base_soc_time_cinb_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("CinB_25cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinB 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_time_cinb_25cm
  
  
  #### SOC - CinputHum
  
  g_kbs_apsim_base_soc_time_cinh_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("CinH_25cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinH 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_time_cinh_25cm
  
  #### SOC - CinputBtoH
  
  g_kbs_apsim_base_soc_time_cinbh_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("CinBtoH_25cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinBtoH 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_time_cinbh_25cm
  
  #### SOC - SoilT
  g_kbs_apsim_base_soc_time_soilt_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SoilT_25cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_time_soilt_25cm
  
  #### SOC - SW
  g_kbs_apsim_base_soc_time_soilw_25cm <- kbs_model_component_apsim_base_time_piv[
    kbs_model_component_apsim_base_time_piv$source %in% c("SW_25cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_time_soilw_25cm
  
  ##### Daycent --------------------------------------
  
  #### SOC - SOC
  
  g_kbs_daycent_base_soc_time_soc_25cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SOC_25cm_pctchg") &
      kbs_model_component_daycent_base_time_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_time_soc_25cm
  
  #### SOC - CI
  g_kbs_daycent_base_soc_time_ci_25cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("CI_pctchg") &
      kbs_model_component_daycent_base_time_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "CI 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_time_ci_25cm
  
  #### SOC - SoilTemp
  g_kbs_daycent_base_soc_time_soilt_25cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SoilT_25cm_pctchg") &
      kbs_model_component_daycent_base_time_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_time_soilt_25cm
  
  #### SOC - SW
  g_kbs_daycent_base_soc_time_soilw_25cm <- kbs_model_component_daycent_base_time_piv[
    kbs_model_component_daycent_base_time_piv$source %in% c("SW_25cm_pctchg") &
      kbs_model_component_daycent_base_time_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_time_soilw_25cm
  
  #### group all N2O plots into one figure
  
  gg_kbs_soc_time_base <- ggarrange(
    g_kbs_apsim_base_soc_time_soc_25cm,
    g_kbs_daycent_base_soc_time_soc_25cm,
    g_kbs_apsim_base_soc_time_cinb_25cm,
    g_kbs_daycent_base_soc_time_ci_25cm,
    g_kbs_apsim_base_soc_time_cinh_25cm,
    NULL,
    g_kbs_apsim_base_soc_time_cinbh_25cm,
    NULL,
    g_kbs_apsim_base_soc_time_soilt_25cm,
    g_kbs_daycent_base_soc_time_soilt_25cm,
    g_kbs_apsim_base_soc_time_soilw_25cm,
    g_kbs_daycent_base_soc_time_soilw_25cm,
    ncol=2,
    nrow=6,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = soc_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = soc_font_size+20, family="serif"))
  
  gg_kbs_soc_time_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_soc_expl_time_base.jpg"),
         plot=gg_kbs_soc_time_base, width=30, height=40, dpi=300)
  
  
  #### LRF-N2O (5cm) ---------------------------------------------
  
  n2o_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  g_lrf_apsim_base_n2o_time_n2o_5cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("N2O_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-5 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_n2o_5cm
  
  
  #### N2O - NO3
  g_lrf_apsim_base_n2o_time_no3_5cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("NO3_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-5 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_no3_5cm
  
  
  #### N2O - SoilT
  g_lrf_apsim_base_n2o_time_soilt_5cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("SoilT_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-5 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_soilt_5cm
  
  #### N2O - SW
  g_lrf_apsim_base_n2o_time_soilw_5cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("SW_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-5 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_soilw_5cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_lrf_daycent_base_n2o_time_n2o_profile <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("N2O_profile_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_time_n2o_profile
  
  
  #### N2O - NO3
  g_lrf_daycent_base_n2o_time_no3_5cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("NO3_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-5 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_time_no3_5cm
  
  #### N2O - SoilTemp not used by Daycent
  
  #### N2O - WFPS
  g_lrf_daycent_base_n2o_time_wfps_5cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("WFPS_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_time_wfps_5cm
  
  #### group all N2O plots into one figure
  
  gg_lrf_n2o_time_5cm_base <- ggarrange(
    g_lrf_apsim_base_n2o_time_n2o_5cm,
    g_lrf_daycent_base_n2o_time_n2o_profile,
    g_lrf_apsim_base_n2o_time_no3_5cm,
    g_lrf_daycent_base_n2o_time_no3_5cm,
    g_lrf_apsim_base_n2o_time_soilt_5cm,
    NULL,
    g_lrf_apsim_base_n2o_time_soilw_5cm,
    g_lrf_daycent_base_n2o_time_wfps_5cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_lrf_n2o_time_5cm_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_time_5cm_base.jpg"),
         plot=gg_lrf_n2o_time_5cm_base, width=30, height=40, dpi=300)
  
  
  #### LRF-N2O (10/15cm) ---------------------------------------------
  
  n2o_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  g_lrf_apsim_base_n2o_time_n2o_15cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("N2O_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 5-15 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_n2o_15cm
  
  
  #### N2O - NO3
  g_lrf_apsim_base_n2o_time_no3_15cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("NO3_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-15 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_no3_15cm
  
  
  #### N2O - SoilT
  g_lrf_apsim_base_n2o_time_soilt_15cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("SoilT_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-15 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_soilt_15cm
  
  #### N2O - SW
  g_lrf_apsim_base_n2o_time_soilw_15cm <- lrf_model_component_apsim_base_time_piv[
    lrf_model_component_apsim_base_time_piv$source %in% c("SW_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 5-15 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_time_soilw_15cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_lrf_daycent_base_n2o_time_n2o_profile <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("N2O_profile_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_time_n2o_profile
  
  
  #### N2O - NO3
  g_lrf_daycent_base_n2o_time_no3_10cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("NO3_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-10 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_time_no3_10cm
  
  #### N2O - SoilTemp not used by Daycent
  
  
  #### N2O - WFPS
  g_lrf_daycent_base_n2o_time_wfps_10cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_time_wfps_10cm
  
  #### group all N2O plots into one figure
  
  gg_lrf_n2o_time_10cm_base <- ggarrange(
    g_lrf_apsim_base_n2o_time_n2o_15cm,
    g_lrf_daycent_base_n2o_time_n2o_profile,
    g_lrf_apsim_base_n2o_time_no3_15cm,
    g_lrf_daycent_base_n2o_time_no3_10cm,
    g_lrf_apsim_base_n2o_time_soilt_15cm,
    NULL,
    g_lrf_apsim_base_n2o_time_soilw_15cm,
    g_lrf_daycent_base_n2o_time_wfps_10cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_lrf_n2o_time_10cm_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_time_10cm_base.jpg"),
         plot=gg_lrf_n2o_time_10cm_base, width=30, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### LRF-CH4 --------------------------------------
  
  ch4_font_size <- 38
  
  ##### Daycent --------------------------------------
  
  #### CH4 - CH4
  g_lrf_daycent_base_ch4_time_ch4_15cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("CH4_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CH"["4"]*" 0-15 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_ch4_15cm
  
  #### CH4 - SoilTemp 2cm
  g_lrf_daycent_base_ch4_time_soilt_2cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("SoilT_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_soilt_2cm
  
  #### CH4 - SoilTemp 5cm
  g_lrf_daycent_base_ch4_time_soilt_5cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("SoilT_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_soilt_5cm
  
  #### CH4 - SoilTemp 10cm
  g_lrf_daycent_base_ch4_time_soilt_10cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("SoilT_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_soilt_10cm
  
  #### CH4 - SoilTemp 20cm
  g_lrf_daycent_base_ch4_time_soilt_20cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("SoilT_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_soilt_20cm
  
  
  #### CH4 - WFPS 2cm
  g_lrf_daycent_base_ch4_time_wfps_2cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("WFPS_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_wfps_2cm
  
  #### CH4 - WFPS 5cm
  g_lrf_daycent_base_ch4_time_wfps_5cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("WFPS_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_wfps_5cm
  
  #### CH4 - WFPS
  g_lrf_daycent_base_ch4_time_wfps_10cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_wfps_10cm
  
  #### CH4 - WFPS 20cm
  g_lrf_daycent_base_ch4_time_wfps_20cm <- lrf_model_component_daycent_base_time_piv[
    lrf_model_component_daycent_base_time_piv$source %in% c("WFPS_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_time_wfps_20cm
  
  #### group all CH4 plots into one figure
  
  gg_lrf_ch4_time_base <- ggarrange(
    g_lrf_daycent_base_ch4_time_ch4_15cm,
    g_lrf_daycent_base_ch4_time_soilt_2cm,
    g_lrf_daycent_base_ch4_time_soilt_5cm,
    g_lrf_daycent_base_ch4_time_soilt_10cm,
    g_lrf_daycent_base_ch4_time_soilt_20cm,
    g_lrf_daycent_base_ch4_time_wfps_2cm,
    g_lrf_daycent_base_ch4_time_wfps_5cm,
    g_lrf_daycent_base_ch4_time_wfps_10cm,
    g_lrf_daycent_base_ch4_time_wfps_20cm,
    ncol=1,
    nrow=9,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 baseline future period (%)",
                                     rot = 90, vjust = 1,
                                     size = ch4_font_size+10, family = "serif"),
                    top = text_grob("Daycent",
                                    size = ch4_font_size+20, family="serif"))
  
  gg_lrf_ch4_time_base
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_ch4_expl_time_base.jpg"),
         plot=gg_lrf_ch4_time_base, width=22, height=45, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  
  ## over time (ukesm) for each management scenario ------------------------------
  #### percent change in means of APSIM model components over time
  #### through the end of the future period in ukesmline climate
  
  n2o_font_size <- 50
  
  #### KBS-N2O --------------------------------------
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  
  g_kbs_apsim_ukesm_n2o_time_n2o_20cm <- kbs_model_component_apsim_ukesm_time_piv[
    kbs_model_component_apsim_ukesm_time_piv$source %in% c("N2O_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-20 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_ukesm_n2o_time_n2o_20cm
  
  #### N2O - NO3
  g_kbs_apsim_ukesm_n2o_time_no3_20cm <- kbs_model_component_apsim_ukesm_time_piv[
    kbs_model_component_apsim_ukesm_time_piv$source %in% c("NO3_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-20 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_ukesm_n2o_time_no3_20cm
  
  
  #### N2O - SoilT
  g_kbs_apsim_ukesm_n2o_time_soilt_20cm <- kbs_model_component_apsim_ukesm_time_piv[
    kbs_model_component_apsim_ukesm_time_piv$source %in% c("SoilT_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-20 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_ukesm_n2o_time_soilt_20cm
  
  #### N2O - SW
  g_kbs_apsim_ukesm_n2o_time_soilw_20cm <- kbs_model_component_apsim_ukesm_time_piv[
    kbs_model_component_apsim_ukesm_time_piv$source %in% c("SW_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-20 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_ukesm_n2o_time_soilw_20cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_kbs_daycent_ukesm_n2o_time_n2o_profile <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("N2O_profile_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_n2o_time_n2o_profile
  
  
  #### N2O - NO3
  g_kbs_daycent_ukesm_n2o_time_no3_10cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("NO3_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-10 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_n2o_time_no3_10cm
  
  #### N2O - SoilTemp not used by Daycent
  
  #### N2O - WFPS
  g_kbs_daycent_ukesm_n2o_time_wfps_10cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_n2o_time_wfps_10cm
  
  #### group all N2O plots into one figure
  
  gg_kbs_n2o_time_ukesm <- ggarrange(
    g_kbs_apsim_ukesm_n2o_time_n2o_20cm,
    g_kbs_daycent_ukesm_n2o_time_n2o_profile,
    g_kbs_apsim_ukesm_n2o_time_no3_20cm,
    g_kbs_daycent_ukesm_n2o_time_no3_10cm,
    g_kbs_apsim_ukesm_n2o_time_soilt_20cm,
    NULL,
    g_kbs_apsim_ukesm_n2o_time_soilw_20cm,
    g_kbs_daycent_ukesm_n2o_time_wfps_10cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 UKESM future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_kbs_n2o_time_ukesm
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_n2o_expl_time_ukesm.jpg"),
         plot=gg_kbs_n2o_time_ukesm, width=30, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### KBS-CH4 --------------------------------------
  
  ch4_font_size <- 38
  
  ##### Daycent --------------------------------------
  
  #### CH4 - CH4
  g_kbs_daycent_ukesm_ch4_time_ch4_15cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("CH4_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CH"["4"]*" 0-15 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_ch4_15cm
  
  #### CH4 - SoilTemp 0-2cm
  g_kbs_daycent_ukesm_ch4_time_soilt_2cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_soilt_2cm
  
  #### CH4 - SoilTemp 2-5cm
  g_kbs_daycent_ukesm_ch4_time_soilt_5cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_soilt_5cm
  
  #### CH4 - SoilTemp 5-10cm
  g_kbs_daycent_ukesm_ch4_time_soilt_10cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_soilt_10cm
  
  #### CH4 - SoilTemp 10-20cm
  g_kbs_daycent_ukesm_ch4_time_soilt_20cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_soilt_20cm

    #### CH4 - WFPS 2cm
  g_kbs_daycent_ukesm_ch4_time_wfps_2cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_wfps_2cm
  
  #### CH4 - WFPS 5cm
  g_kbs_daycent_ukesm_ch4_time_wfps_5cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_wfps_5cm
  
  #### CH4 - WFPS 10cm
  g_kbs_daycent_ukesm_ch4_time_wfps_10cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_wfps_10cm
  
  #### CH4 - WFPS 20cm
  g_kbs_daycent_ukesm_ch4_time_wfps_20cm <- kbs_model_component_daycent_ukesm_time_piv[
    kbs_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_ukesm_ch4_time_wfps_20cm
  
  #### group all CH4 plots into one figure
  
  gg_kbs_ch4_time_ukesm <- ggarrange(
    g_kbs_daycent_ukesm_ch4_time_ch4_15cm,
    g_kbs_daycent_ukesm_ch4_time_soilt_2cm,
    g_kbs_daycent_ukesm_ch4_time_soilt_5cm,
    g_kbs_daycent_ukesm_ch4_time_soilt_10cm,
    g_kbs_daycent_ukesm_ch4_time_soilt_20cm,
    g_kbs_daycent_ukesm_ch4_time_wfps_2cm,
    g_kbs_daycent_ukesm_ch4_time_wfps_5cm,
    g_kbs_daycent_ukesm_ch4_time_wfps_10cm,
    g_kbs_daycent_ukesm_ch4_time_wfps_20cm,
    ncol=1,
    nrow=9,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 UKESM future period (%)",
                                     rot = 90, vjust = 1,
                                     size = ch4_font_size+10, family = "serif"),
                    top = text_grob("Daycent",
                                    size = ch4_font_size+20, family="serif"))
  
  gg_kbs_ch4_time_ukesm
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_ch4_expl_time_ukesm.jpg"),
         plot=gg_kbs_ch4_time_ukesm, width=22, height=40, dpi=300)  
  
  #**********************************************
  #**********************************************
  
  #### LRF-N2O (5cm) ---------------------------------------------
  
  n2o_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  g_lrf_apsim_ukesm_n2o_time_n2o_5cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("N2O_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-5 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_n2o_5cm
  
  
  #### N2O - NO3
  g_lrf_apsim_ukesm_n2o_time_no3_5cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("NO3_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-5 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_no3_5cm
  
  
  #### N2O - SoilT
  g_lrf_apsim_ukesm_n2o_time_soilt_5cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("SoilT_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-5 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_soilt_5cm
  
  #### N2O - SW
  g_lrf_apsim_ukesm_n2o_time_soilw_5cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("SW_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-5 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_soilw_5cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_lrf_daycent_ukesm_n2o_time_n2o_profile <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("N2O_profile_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_n2o_time_n2o_profile
  
  
  #### N2O - NO3
  g_lrf_daycent_ukesm_n2o_time_no3_5cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("NO3_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-5 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_n2o_time_no3_5cm
  
  #### N2O - SoilTemp not used by Daycent
  
  #### N2O - WFPS
  g_lrf_daycent_ukesm_n2o_time_wfps_5cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_n2o_time_wfps_5cm
  
  #### group all N2O plots into one figure
  
  gg_lrf_n2o_time_5cm_ukesm <- ggarrange(
    g_lrf_apsim_ukesm_n2o_time_n2o_5cm,
    g_lrf_daycent_ukesm_n2o_time_n2o_profile,
    g_lrf_apsim_ukesm_n2o_time_no3_5cm,
    g_lrf_daycent_ukesm_n2o_time_no3_5cm,
    g_lrf_apsim_ukesm_n2o_time_soilt_5cm,
    NULL,
    g_lrf_apsim_ukesm_n2o_time_soilw_5cm,
    g_lrf_daycent_ukesm_n2o_time_wfps_5cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 UKESM future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_lrf_n2o_time_5cm_ukesm
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_time_5cm_ukesm.jpg"),
         plot=gg_lrf_n2o_time_5cm_ukesm, width=30, height=40, dpi=300)
  
  
  #### LRF-N2O (10/15cm) ---------------------------------------------
  
  n2o_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  g_lrf_apsim_ukesm_n2o_time_n2o_15cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("N2O_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 5-15 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_n2o_15cm
  
  
  #### N2O - NO3
  g_lrf_apsim_ukesm_n2o_time_no3_15cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("NO3_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-15 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_no3_15cm
  
  
  #### N2O - SoilT
  g_lrf_apsim_ukesm_n2o_time_soilt_15cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("SoilT_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-15 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_soilt_15cm
  
  #### N2O - SW
  g_lrf_apsim_ukesm_n2o_time_soilw_15cm <- lrf_model_component_apsim_ukesm_time_piv[
    lrf_model_component_apsim_ukesm_time_piv$source %in% c("SW_15cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 5-15 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_ukesm_n2o_time_soilw_15cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_lrf_daycent_ukesm_n2o_time_n2o_profile <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("N2O_profile_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_n2o_time_n2o_profile
  
  
  #### N2O - NO3
  g_lrf_daycent_ukesm_n2o_time_no3_10cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("NO3_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-10 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_n2o_time_no3_10cm
  
  #### N2O - SoilTemp not used by Daycent
  
  
  #### N2O - WFPS
  g_lrf_daycent_ukesm_n2o_time_wfps_10cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_n2o_time_wfps_10cm
  
  #### group all N2O plots into one figure
  
  gg_lrf_n2o_time_10cm_ukesm <- ggarrange(
    g_lrf_apsim_ukesm_n2o_time_n2o_15cm,
    g_lrf_daycent_ukesm_n2o_time_n2o_profile,
    g_lrf_apsim_ukesm_n2o_time_no3_15cm,
    g_lrf_daycent_ukesm_n2o_time_no3_10cm,
    g_lrf_apsim_ukesm_n2o_time_soilt_15cm,
    NULL,
    g_lrf_apsim_ukesm_n2o_time_soilw_15cm,
    g_lrf_daycent_ukesm_n2o_time_wfps_10cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 UKESM future period (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_lrf_n2o_time_10cm_ukesm
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_time_10cm_ukesm.jpg"),
         plot=gg_lrf_n2o_time_10cm_ukesm, width=30, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### LRF-CH4 --------------------------------------
  
  ch4_font_size <- 38
  
  ##### Daycent --------------------------------------
  
  #### CH4 - CH4
  g_lrf_daycent_ukesm_ch4_time_ch4_15cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("CH4_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CH"["4"]*" 0-15 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_ch4_15cm
  
  #### CH4 - SoilTemp 2cm
  g_lrf_daycent_ukesm_ch4_time_soilt_2cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_soilt_2cm

  #### CH4 - SoilTemp 5cm
  g_lrf_daycent_ukesm_ch4_time_soilt_5cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_soilt_5cm

    #### CH4 - SoilTemp 10cm
  g_lrf_daycent_ukesm_ch4_time_soilt_10cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_soilt_10cm
  
  #### CH4 - SoilTemp 20cm
  g_lrf_daycent_ukesm_ch4_time_soilt_20cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("SoilT_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_soilt_20cm
  
  #### CH4 - WFPS 2cm
  g_lrf_daycent_ukesm_ch4_time_wfps_2cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_2cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_wfps_2cm
  
  #### CH4 - WFPS 5cm
  g_lrf_daycent_ukesm_ch4_time_wfps_5cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_5cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_wfps_5cm
  
  #### CH4 - WFPS
  g_lrf_daycent_ukesm_ch4_time_wfps_10cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_10cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_wfps_10cm
  
  #### CH4 - WFPS 20cm
  g_lrf_daycent_ukesm_ch4_time_wfps_20cm <- lrf_model_component_daycent_ukesm_time_piv[
    lrf_model_component_daycent_ukesm_time_piv$source %in% c("WFPS_20cm_pctchg"),] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-CR","RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_color,RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_ukesm_ch4_time_wfps_20cm
  
  #### group all CH4 plots into one figure
  
  gg_lrf_ch4_time_ukesm <- ggarrange(
    g_lrf_daycent_ukesm_ch4_time_ch4_15cm,
    g_lrf_daycent_ukesm_ch4_time_soilt_2cm,
    g_lrf_daycent_ukesm_ch4_time_soilt_5cm,
    g_lrf_daycent_ukesm_ch4_time_soilt_10cm,
    g_lrf_daycent_ukesm_ch4_time_soilt_20cm,
    g_lrf_daycent_ukesm_ch4_time_wfps_2cm,
    g_lrf_daycent_ukesm_ch4_time_wfps_5cm,
    g_lrf_daycent_ukesm_ch4_time_wfps_10cm,
    g_lrf_daycent_ukesm_ch4_time_wfps_20cm,
    ncol=1,
    nrow=9,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent change over 2022-2050 UKESM future period (%)",
                                     rot = 90, vjust = 1,
                                     size = ch4_font_size+10, family = "serif"),
                    top = text_grob("Daycent",
                                    size = ch4_font_size+20, family="serif"))
  
  gg_lrf_ch4_time_ukesm
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_ch4_expl_time_ukesm.jpg"),
         plot=gg_lrf_ch4_time_ukesm, width=22, height=45, dpi=300)
  
  
  
  #**********************************************************************
  #**********************************************************************

  ## by management scenario ---------------------------------------------
  #### percent change in means of APSIM model components compared to the
  #### baseline mgmt scenario (CR), through the end of the future period
  
  ############# Note: these are not as helpful as it sounds #############
  
  n2o_font_size <- 50
  
  #### KBS-N2O --------------------------------------
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  
  g_kbs_apsim_base_n2o_bymgmt_n2o_20cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("N2O_20cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-20 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_bymgmt_n2o_20cm
  
  
  #### N2O - NO3
  g_kbs_apsim_base_n2o_bymgmt_no3_20cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("NO3_20cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-20 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_bymgmt_no3_20cm
  
  
  #### N2O - SoilT
  g_kbs_apsim_base_n2o_bymgmt_soilt_20cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("SoilT_20cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-20 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_bymgmt_soilt_20cm
  
  #### N2O - SW
  g_kbs_apsim_base_n2o_bymgmt_soilw_20cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("SW_20cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-20 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_bymgmt_soilw_20cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_kbs_daycent_base_n2o_bymgmt_n2o_profile <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("N2O_profile_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_n2o_bymgmt_n2o_profile
  
  
  #### N2O - NO3
  g_kbs_daycent_base_n2o_bymgmt_no3_10cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("NO3_10cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-10 cm"))) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_n2o_bymgmt_no3_10cm
  
  #### N2O - SoilTemp not used by Daycent
  
  #### N2O - WFPS
  g_kbs_daycent_base_n2o_bymgmt_wfps_10cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_10cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_n2o_bymgmt_wfps_10cm
  
  #### group all N2O plots into one figure
  
  gg_kbs_n2o_bymgmt <- ggarrange(
    g_kbs_apsim_base_n2o_bymgmt_n2o_20cm,
    g_kbs_daycent_base_n2o_bymgmt_n2o_profile,
    g_kbs_apsim_base_n2o_bymgmt_no3_20cm,
    g_kbs_daycent_base_n2o_bymgmt_no3_10cm,
    g_kbs_apsim_base_n2o_bymgmt_soilt_20cm,
    NULL,
    g_kbs_apsim_base_n2o_bymgmt_soilw_20cm,
    g_kbs_daycent_base_n2o_bymgmt_wfps_10cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_kbs_n2o_bymgmt
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_n2o_expl_bymgmt.jpg"),
         plot=gg_kbs_n2o_bymgmt, width=30, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### KBS-CH4 --------------------------------------
  
  ch4_font_size <- 38
  
  ##### Daycent --------------------------------------
  
  #### CH4 - CH4
  g_kbs_daycent_base_ch4_bymgmt_ch4_15cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("CH4_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CH"["4"]*" 0-15 cm"))) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_bymgmt_ch4_15cm
  
  #### CH4 - SoilTemp
  g_kbs_daycent_base_ch4_bymgmt_soilt_10cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("SoilT_10cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_bymgmt_soilt_10cm
  
  
  #### CH4 - WFPS 2cm
  g_kbs_daycent_base_ch4_bymgmt_wfps_2cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_2cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_bymgmt_wfps_2cm
  
  #### CH4 - WFPS 5cm
  g_kbs_daycent_base_ch4_bymgmt_wfps_5cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_5cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_bymgmt_wfps_5cm
  
  #### CH4 - WFPS
  g_kbs_daycent_base_ch4_bymgmt_wfps_10cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_10cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_bymgmt_wfps_10cm
  
  #### CH4 - WFPS 20cm
  g_kbs_daycent_base_ch4_bymgmt_wfps_20cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_20cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_ch4_bymgmt_wfps_20cm
  
  #### group all CH4 plots into one figure
  
  gg_kbs_ch4_bymgmt <- ggarrange(
    g_kbs_daycent_base_ch4_bymgmt_ch4_15cm,
    g_kbs_daycent_base_ch4_bymgmt_soilt_10cm,
    g_kbs_daycent_base_ch4_bymgmt_wfps_2cm,
    g_kbs_daycent_base_ch4_bymgmt_wfps_5cm,
    g_kbs_daycent_base_ch4_bymgmt_wfps_10cm,
    g_kbs_daycent_base_ch4_bymgmt_wfps_20cm,
    ncol=1,
    nrow=6,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = ch4_font_size+10, family = "serif"),
                    top = text_grob("Daycent",
                                    size = ch4_font_size+20, family="serif"))
  
  gg_kbs_ch4_bymgmt
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_ch4_expl_bymgmt.jpg"),
         plot=gg_kbs_ch4_bymgmt, width=22, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### KBS-SOC --------------------------------------
  ########### NEED TO ADD MILLENNIAL AND ROTHC ###########
  
  soc_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### SOC - SOC
  
  g_kbs_apsim_base_soc_bymgmt_soc_25cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("SOC_25cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_bymgmt_soc_25cm
  
  #### SOC - CinputBio
  
  g_kbs_apsim_base_soc_bymgmt_cinb_25cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("CinB_25cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinB 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_bymgmt_cinb_25cm
  
  
  #### SOC - CinputHum
  
  g_kbs_apsim_base_soc_bymgmt_cinh_25cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("CinH_25cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinH 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_bymgmt_cinh_25cm
  
  #### SOC - CinputBtoH
  
  g_kbs_apsim_base_soc_bymgmt_cinbh_25cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("CinBtoH_25cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinBtoH 0-25 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_bymgmt_cinbh_25cm
  
  #### SOC - SoilT
  g_kbs_apsim_base_soc_bymgmt_soilt_25cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("SoilT_25cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_bymgmt_soilt_25cm
  
  #### SOC - SW
  g_kbs_apsim_base_soc_bymgmt_soilw_25cm <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("SW_25cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-25 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                               "CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,BC96_color,
                               CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_soc_bymgmt_soilw_25cm
  
  
  ##### Daycent --------------------------------------
  
  #### SOC - SOC
  
  g_kbs_daycent_base_soc_bymgmt_soc_25cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("SOC_25cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_bymgmt_soc_25cm
  
  #### SOC - CI
  g_kbs_daycent_base_soc_bymgmt_ci_25cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("CI_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "CI 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_bymgmt_ci_25cm
  
  #### SOC - SoilTemp
  g_kbs_daycent_base_soc_bymgmt_soilt_25cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("SoilT_25cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_bymgmt_soilt_25cm
  
  #### SOC - SW
  g_kbs_daycent_base_soc_bymgmt_soilw_25cm <- kbs_model_component_daycent_base_bymgmt_piv[
    kbs_model_component_daycent_base_bymgmt_piv$source %in% c("SW_25cm_pctchg") &
      kbs_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-25 cm")) +
    scale_fill_manual(labels=c("CC-CR","NT-CR","RF05-CR","RF15-CR",
                               "RF25-CR","RF35-CR","RR00-CR",
                               "RR25-CR","RR50-CR"),
                      values=c(CC_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,
                               RR25_color,RR50_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_daycent_base_soc_bymgmt_soilw_25cm
  
  #### group all SOC plots into one figure
  
  gg_kbs_soc_bymgmt <- ggarrange(
    g_kbs_apsim_base_soc_bymgmt_soc_25cm,
    g_kbs_daycent_base_soc_bymgmt_soc_25cm,
    g_kbs_apsim_base_soc_bymgmt_cinb_25cm,
    g_kbs_daycent_base_soc_bymgmt_ci_25cm,
    g_kbs_apsim_base_soc_bymgmt_cinh_25cm,
    NULL,
    g_kbs_apsim_base_soc_bymgmt_cinbh_25cm,
    NULL,
    g_kbs_apsim_base_soc_bymgmt_soilt_25cm,
    g_kbs_daycent_base_soc_bymgmt_soilt_25cm,
    g_kbs_apsim_base_soc_bymgmt_soilw_25cm,
    g_kbs_daycent_base_soc_bymgmt_soilw_25cm,
    ncol=2,
    nrow=6,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = soc_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = soc_font_size+20, family="serif"))
  
  gg_kbs_soc_bymgmt
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_soc_expl_bymgmt.jpg"),
         plot=gg_kbs_soc_bymgmt, width=30, height=45, dpi=300)
  
  
  #### LRF-N2O (5cm) ---------------------------------------------
  
  n2o_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  g_lrf_apsim_base_n2o_bymgmt_n2o_5cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("N2O_5cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-5 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_n2o_5cm
  
  
  #### N2O - NO3
  g_lrf_apsim_base_n2o_bymgmt_no3_5cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("NO3_5cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-5 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_no3_5cm
  
  
  #### N2O - SoilT
  g_lrf_apsim_base_n2o_bymgmt_soilt_5cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SoilT_5cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-5 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_soilt_5cm
  
  #### N2O - SW
  g_lrf_apsim_base_n2o_bymgmt_soilw_5cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SW_5cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-5 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_soilw_5cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_lrf_daycent_base_n2o_bymgmt_n2o_profile <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("N2O_profile_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_bymgmt_n2o_profile
  
  
  #### N2O - NO3
  g_lrf_daycent_base_n2o_bymgmt_no3_5cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("NO3_5cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 0-5 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_bymgmt_no3_5cm
  
  #### N2O - SoilTemp not used by Daycent
  
  #### N2O - WFPS
  g_lrf_daycent_base_n2o_bymgmt_wfps_5cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_5cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_bymgmt_wfps_5cm
  
  #### group all N2O plots into one figure
  
  gg_lrf_n2o_bymgmt_5cm <- ggarrange(
    g_lrf_apsim_base_n2o_bymgmt_n2o_5cm,
    g_lrf_daycent_base_n2o_bymgmt_n2o_profile,
    g_lrf_apsim_base_n2o_bymgmt_no3_5cm,
    g_lrf_daycent_base_n2o_bymgmt_no3_5cm,
    g_lrf_apsim_base_n2o_bymgmt_soilt_5cm,
    NULL,
    g_lrf_apsim_base_n2o_bymgmt_soilw_5cm,
    g_lrf_daycent_base_n2o_bymgmt_wfps_5cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline RR00-CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_lrf_n2o_bymgmt_5cm
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_bymgmt_5cm.jpg"),
         plot=gg_lrf_n2o_bymgmt_5cm, width=30, height=40, dpi=300)
  
  
  #### LRF-N2O (10/15cm) ---------------------------------------------
  
  n2o_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### N2O - N2O
  g_lrf_apsim_base_n2o_bymgmt_n2o_15cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("N2O_15cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 5-15 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_n2o_15cm
  
  
  #### N2O - NO3
  g_lrf_apsim_base_n2o_bymgmt_no3_15cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("NO3_15cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-15 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_no3_15cm
  
  
  #### N2O - SoilT
  g_lrf_apsim_base_n2o_bymgmt_soilt_15cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SoilT_15cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-15 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_soilt_15cm
  
  #### N2O - SW
  g_lrf_apsim_base_n2o_bymgmt_soilw_15cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SW_15cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 5-15 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_n2o_bymgmt_soilw_15cm
  
  ##### Daycent --------------------------------------
  
  #### N2O - N2O
  g_lrf_daycent_base_n2o_bymgmt_n2o_profile <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("N2O_profile_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("N"["2"]*"O 0-200 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_bymgmt_n2o_profile
  
  
  #### N2O - NO3
  g_lrf_daycent_base_n2o_bymgmt_no3_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("NO3_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("NO"["3"]*" 5-10 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_bymgmt_no3_10cm
  
  #### N2O - SoilTemp not used by Daycent
  
  
  #### N2O - WFPS
  g_lrf_daycent_base_n2o_bymgmt_wfps_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent N2O Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = n2o_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_n2o_bymgmt_wfps_10cm
  
  #### group all N2O plots into one figure
  
  gg_lrf_n2o_bymgmt_10cm <- ggarrange(
    g_lrf_apsim_base_n2o_bymgmt_n2o_15cm,
    g_lrf_daycent_base_n2o_bymgmt_n2o_profile,
    g_lrf_apsim_base_n2o_bymgmt_no3_15cm,
    g_lrf_daycent_base_n2o_bymgmt_no3_10cm,
    g_lrf_apsim_base_n2o_bymgmt_soilt_15cm,
    NULL,
    g_lrf_apsim_base_n2o_bymgmt_soilw_15cm,
    g_lrf_daycent_base_n2o_bymgmt_wfps_10cm,
    ncol=2,
    nrow=4,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline RR00-CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = n2o_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = n2o_font_size+20, family="serif"))
  
  gg_lrf_n2o_bymgmt_10cm
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_bymgmt_10cm.jpg"),
         plot=gg_lrf_n2o_bymgmt_10cm, width=30, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### LRF-CH4 --------------------------------------
  
  ch4_font_size <- 38
  
  ##### Daycent --------------------------------------
  
  #### CH4 - CH4
  g_lrf_daycent_base_ch4_bymgmt_ch4_15cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("CH4_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CH"["4"]*" 0-15 cm"))) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_bymgmt_ch4_15cm
  
  #### CH4 - SoilTemp
  g_lrf_daycent_base_ch4_bymgmt_soilt_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("SoilT_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_bymgmt_soilt_10cm
  
  
  #### CH4 - WFPS 2cm
  g_lrf_daycent_base_ch4_bymgmt_wfps_2cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_2cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 0-2 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_bymgmt_wfps_2cm
  
  #### CH4 - WFPS 5cm
  g_lrf_daycent_base_ch4_bymgmt_wfps_5cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_5cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 2-5 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_bymgmt_wfps_5cm
  
  #### CH4 - WFPS
  g_lrf_daycent_base_ch4_bymgmt_wfps_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_bymgmt_wfps_10cm
  
  #### CH4 - WFPS 20cm
  g_lrf_daycent_base_ch4_bymgmt_wfps_20cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("WFPS_20cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent CH4 Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = ch4_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_ch4_bymgmt_wfps_20cm
  
  #### group all CH4 plots into one figure
  
  gg_lrf_ch4_bymgmt <- ggarrange(
    g_lrf_daycent_base_ch4_bymgmt_ch4_15cm,
    g_lrf_daycent_base_ch4_bymgmt_soilt_10cm,
    g_lrf_daycent_base_ch4_bymgmt_wfps_2cm,
    g_lrf_daycent_base_ch4_bymgmt_wfps_5cm,
    g_lrf_daycent_base_ch4_bymgmt_wfps_10cm,
    g_lrf_daycent_base_ch4_bymgmt_wfps_20cm,
    ncol=1,
    nrow=6,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline RR00-CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = ch4_font_size+10, family = "serif"),
                    top = text_grob("Daycent",
                                    size = ch4_font_size+20, family="serif"))
  
  gg_lrf_ch4_bymgmt
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_ch4_expl_bymgmt.jpg"),
         plot=gg_lrf_ch4_bymgmt, width=22, height=40, dpi=300)
  
  
  #**********************************************
  #**********************************************
  
  #### LRF-SOC --------------------------------------
  ########### NEED TO ADD MILLENNIAL AND ROTHC ###########
  
  soc_font_size <- 50
  
  ##### APSIM --------------------------------------
  
  #### SOC - SOC
  
  g_lrf_apsim_base_soc_bymgmt_soc_10cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SOC_10cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 0-10 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_soc_bymgmt_soc_10cm
  
  #### SOC - CinputBio
  
  g_lrf_apsim_base_soc_bymgmt_cinb_10cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("CinB_10cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinB 0-10 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_soc_bymgmt_cinb_10cm
  
  
  #### SOC - CinputHum
  
  g_lrf_apsim_base_soc_bymgmt_cinh_10cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("CinH_10cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinH 0-10 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_soc_bymgmt_cinh_10cm
  
  #### SOC - CinputBtoH
  
  g_lrf_apsim_base_soc_bymgmt_cinbh_10cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("CinBtoH_10cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key apsim SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = bquote("CinBtoH 0-10 cm"))) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_soc_bymgmt_cinbh_10cm
  
  #### SOC - SoilT
  g_lrf_apsim_base_soc_bymgmt_soilt_10cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SoilT_10cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-10 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_soc_bymgmt_soilt_10cm
  
  #### SOC - SW
  g_lrf_apsim_base_soc_bymgmt_soilw_10cm <- lrf_model_component_apsim_base_bymgmt_piv[
    lrf_model_component_apsim_base_bymgmt_piv$source %in% c("SW_10cm_pctchg") &
      lrf_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key APSIM soc Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-10 cm")) +
    scale_fill_manual(labels=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR",
                               "BC96-CR","CC-CR","CC-NT-CR","CN",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR",
                               "RR50-CR","RR50-NT-CR"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,
                               RR50_color,RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_apsim_base_soc_bymgmt_soilw_10cm
  
  ##### Daycent --------------------------------------
  
  #### SOC - SOC
  
  g_lrf_daycent_base_soc_bymgmt_soc_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("SOC_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SOC 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_soc_bymgmt_soc_10cm
  
  #### SOC - CI
  g_lrf_daycent_base_soc_bymgmt_ci_20cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("CI_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "CI 0-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_soc_bymgmt_ci_20cm
  
  #### SOC - SoilTemp
  g_lrf_daycent_base_soc_bymgmt_soilt_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("SoilT_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_soc_bymgmt_soilt_10cm
  
  #### SOC - SW
  g_lrf_daycent_base_soc_bymgmt_soilw_10cm <- lrf_model_component_daycent_base_bymgmt_piv[
    lrf_model_component_daycent_base_bymgmt_piv$source %in% c("SW_10cm_pctchg") &
      lrf_model_component_daycent_base_bymgmt_piv$Scenario_Abbrev != "RR00-CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    #ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    #ggtitle("Key Daycent SOC Model Components") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 5-10 cm")) +
    scale_fill_manual(labels=c("CC-CR","CC-NT-CR","CN","NT-CR",
                               "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                               "RR00-NT-CR","RR25-CR","RR25-NT-CR","RR50-CR",
                               "RR50-NT-CR"),
                      values=c(CC_color,CC_NT_color,CN_color,NT_color,
                               RF05_color,RF15_color,RF25_color,RF35_color,
                               RR00_NT_color,RR25_color,RR25_NT_color,RR50_color,
                               RR50_NT_color),
                      name="Scenario") +
    theme_classic(base_family = "serif", base_size = soc_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          legend.position = "none",
          legend.key = element_blank())
  
  g_lrf_daycent_base_soc_bymgmt_soilw_10cm
  
  #### group all SOC plots into one figure
  
  gg_lrf_soc_bymgmt <- ggarrange(
    g_lrf_apsim_base_soc_bymgmt_soc_10cm,
    g_lrf_daycent_base_soc_bymgmt_soc_10cm,
    g_lrf_apsim_base_soc_bymgmt_cinb_10cm,
    g_lrf_daycent_base_soc_bymgmt_ci_20cm,
    g_lrf_apsim_base_soc_bymgmt_cinh_10cm,
    NULL,
    g_lrf_apsim_base_soc_bymgmt_cinbh_10cm,
    NULL,
    g_lrf_apsim_base_soc_bymgmt_soilt_10cm,
    g_lrf_daycent_base_soc_bymgmt_soilt_10cm,
    g_lrf_apsim_base_soc_bymgmt_soilw_10cm,
    g_lrf_daycent_base_soc_bymgmt_soilw_10cm,
    ncol=2,
    nrow=6,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline RR00-CR Mgmt (%)",
                                     rot = 90, vjust = 1,
                                     size = soc_font_size+10, family = "serif"),
                    top = text_grob("APSIM                                               Daycent",
                                    size = soc_font_size+20, family="serif"))
  
  gg_lrf_soc_bymgmt
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_soc_expl_bymgmt.jpg"),
         plot=gg_lrf_soc_bymgmt, width=30, height=45, dpi=300)
  
  
  ## end mgmt graphs ----------------------------------------------------------
  
  
  
  
  g_kbs_apsim_base_n2o_bymgmt <- kbs_model_component_apsim_base_bymgmt_piv[
    kbs_model_component_apsim_base_bymgmt_piv$source %in% c("N2O_20cm_pctchg",
                                                            "NO3_20cm_pctchg",
                                                            "SoilT_20cm_pctchg",
                                                            "SW_20cm_pctchg") &
      kbs_model_component_apsim_base_bymgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Management (%)") +
    xlab("") +
    ggtitle("Key APSIM N2O Model Components") +
    scale_fill_manual(labels=c("BC19_color","BC38_color","BC57_color","BC76_color",
                               "BC96_color","CC_color","CC_NT_color","CN_color",
                               "CR_color","NT_color","RF05_color","RF15_color",
                               "RF25_color","RF35_color","RR00_color","RR00_NT_color",
                               "RR25_color","RR25_NT_color","RR50_color","RR50_NT_color"),
                      values=c(BC19_color,BC38_color,BC57_color,BC76_color,
                               BC96_color,CC_color,CC_NT_color,CN_color,
                               CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,RR00_NT_color,
                               RR25_color,RR25_NT_color,RR50_color,RR50_NT_color),
                      name="Component") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_kbs_apsim_base_n2o_bymgmt
  
  
  #### percent change in means of APSIM model components compared to the
  #### baseline mgmt scenario, through the end of the future period
  g_daycent_n2o_bymgmt <- model_component_means_daycent_mgmt_piv[
    model_component_means_daycent_mgmt_piv$source %in% c("N2O","NO3","SoilTemp",
                                                         "WFPS_10cm","WFPS_20cm",
                                                         "WFPS_40cm") &
      model_component_means_daycent_mgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=Scenario_Abbrev)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline mgmt (%)") +
    xlab("") +
    ggtitle("Key Daycent N2O Model Components") +
    scale_fill_manual(labels=c("CC_color","CC_NT_color","CN_color",
                               "CR_color","NT_color","RF05_color","RF15_color",
                               "RF25_color","RF35_color","RR00_color","RR00_NT_color",
                               "RR25_color","RR25_NT_color","RR50_color","RR50_NT_color"),
                      values=c(CC_color,CC_NT_color,CN_color,
                               CR_color,NT_color,RF05_color,RF15_color,
                               RF25_color,RF35_color,RR00_color,RR00_NT_color,
                               RR25_color,RR25_NT_color,RR50_color,RR50_NT_color),
                      name="Component") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_daycent_n2o_bymgmt
  
  #### percent change in means of APSIM model components compared to the
  #### baseline mgmt scenario, through the end of the future period
  g_daycent_ch4_bymgmt <- model_component_means_daycent_mgmt_piv[
    model_component_means_daycent_mgmt_piv$source %in% c("CH4","SoilTemp",
                                                         "WFPS_0cm","WFPS_2cm",
                                                         "WFPS_5cm","WFPS_10cm") &
      model_component_means_daycent_mgmt_piv$Scenario_Abbrev != "CR",] %>%
    ggplot(aes(x=Scenario_Abbrev, y=vals, fill=source)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline mgmt (%)") +
    xlab("") +
    ggtitle("Key Daycent CH4 Model Components") +
    scale_fill_manual(labels=c("CH4","Soil Temp","WFPS 0-2 cm",
                               "WFPS 2-5 cm", "WFPS 5-10 cm",
                               "WFPS 10-20 cm"),
                      values=c(CH4_color,SoilT_color,WFPS_0cm_color,
                               WFPS_2cm_color,WFPS_5cm_color,
                               WFPS_10cm_color),
                      name="Component") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_daycent_ch4_bymgmt
  
  
  ggsave(filename=paste0(these_results_folder,"/pub_apsim_n2o_expl_bymgmt.jpg"),
         plot=g_apsim_n2o_bymgmt, width=18, height=20, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_daycent_n2o_expl_bymgmt.jpg"),
         plot=g_daycent_n2o_bymgmt, width=18, height=20, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_daycent_ch4_expl_bymgmt.jpg"),
         plot=g_daycent_ch4_bymgmt, width=18, height=20, dpi=300)
  
  
  
  ## by climate scenario ---------------------------------------------
  
  ### model change in APSIM model components 
  
  g_kbs_apsim_base_CR_soc <- kbs_model_components[kbs_model_components$Model=="APSIM" &
                                                    kbs_model_components$Climate_Scenario==1 &
                                                    kbs_model_components$Mgmt_Scenario==3,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key APSIM N2O Model Components") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  #### percent change in means of APSIM model components compared to the
  #### baseline climate scenario, through the end of the future period
  # g_apsim_n2o_byclimate <- model_component_means_apsim_climate_piv[
  #   model_component_means_apsim_climate_piv$source %in% c("N2O","NO3","SoilTemp",
  #                                                     "SW_20cm","SW_40cm","SW_60cm") &
  #     model_component_means_apsim_climate_piv$Climate_Scenario != 1,] %>%
  #   ggplot(aes(x=source, y=vals, fill=source)) +
  #   geom_col(position="dodge",colour=NA) +
  #   ylab("Percent Change from Baseline Climate (%)") +
  #   xlab("") +
  #   ggtitle("Key APSIM N2O Model Components") +
  #   scale_fill_manual(labels=c("N2O","NO3","Soil Temp","SW 0-20 cm",
  #                              "SW 20-40 cm", "SW 40-60 cm"),
  #                     values=c(N2O_color,NO3_color,SoilT_color,
  #                              SW_20cm_color,SW_40cm_color,SW_60cm_color),
  #                     name="Component") +
  #   facet_grid(climate_desc~site_name) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         panel.border = element_rect(colour = "darkgrey", fill=NA),
  #         strip.background = element_blank(),
  #         axis.line = element_line(),
  #         axis.text.x = element_text(angle = 45,
  #                                    hjust = 1),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # g_apsim_n2o_byclimate
  
  g_apsim_n2o_byclimate <- model_component_means_apsim_climate_piv[
    model_component_means_apsim_climate_piv$source %in% c("N2O","NO3","SoilTemp",
                                                          "SW_20cm","SW_40cm","SW_60cm") &
      model_component_means_apsim_climate_piv$Climate_Scenario != 1,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key APSIM N2O Model Components") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_apsim_n2o_byclimate
  
  #### percent change in means of APSIM model components compared to the
  #### baseline climate scenario, through the end of the future period
  # g_daycent_n2o_byclimate <- model_component_means_daycent_climate_piv[
  #   model_component_means_daycent_climate_piv$source %in% c("N2O","NO3","SoilTemp",
  #                                                         "WFPS_10cm","WFPS_20cm",
  #                                                         "WFPS_40cm") &
  #     model_component_means_daycent_climate_piv$Climate_Scenario != 1,] %>%
  #   ggplot(aes(x=source, y=vals, fill=source)) +
  #   geom_col(position="dodge",colour=NA) +
  #   ylab("Percent Change from Baseline Climate (%)") +
  #   xlab("") +
  #   ggtitle("Key Daycent N2O Model Components") +
  #   scale_fill_manual(labels=c("N2O","NO3","Soil Temp","WFPS 10-20 cm",
  #                              "WFPS 20-40 cm", "WFPS 40-60 cm"),
  #                     values=c(N2O_color,NO3_color,SoilT_color,
  #                              WFPS_10cm_color,WFPS_20cm_color,WFPS_40cm_color),
  #                     name="Component") +
  #   facet_grid(climate_desc~site_name) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         panel.border = element_rect(colour = "darkgrey", fill=NA),
  #         strip.background = element_blank(),
  #         axis.line = element_line(),
  #         axis.text.x = element_text(angle = 45,
  #                                    hjust = 1),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # g_daycent_n2o_byclimate
  
  g_daycent_n2o_byclimate <- model_component_means_daycent_climate_piv[
    model_component_means_daycent_climate_piv$source %in% c("N2O","NO3","SoilTemp",
                                                            "WFPS_10cm","WFPS_20cm",
                                                            "WFPS_40cm") &
      model_component_means_daycent_climate_piv$Climate_Scenario != 1,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key Daycent N2O Model Components") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_daycent_n2o_byclimate
  
  #### percent change in means of APSIM model components compared to the
  #### baseline climate scenario, through the end of the future period
  # g_daycent_ch4_byclimate <- model_component_means_daycent_climate_piv[
  #   model_component_means_daycent_climate_piv$source %in% c("CH4","SoilTemp",
  #                                                           "WFPS_0cm","WFPS_2cm",
  #                                                           "WFPS_5cm") &
  #     model_component_means_daycent_climate_piv$Climate_Scenario != 1,] %>%
  #   ggplot(aes(x=source, y=vals, fill=source)) +
  #   geom_col(position="dodge",colour=NA) +
  #   ylab("Percent Change from Baseline Climate (%)") +
  #   xlab("") +
  #   ggtitle("Key Daycent CH4 Model Components") +
  #   scale_fill_manual(labels=c("CH4","Soil Temp","WFPS 0-2.5 cm",
  #                              "WFPS 2.5-5 cm", "WFPS 5-10 cm"),
  #                     values=c(CH4_color,SoilT_color,WFPS_0cm_color,
  #                              WFPS_2cm_color,WFPS_5cm_color),
  #                     name="Component") +
  #   facet_grid(climate_desc~site_name) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         panel.border = element_rect(colour = "darkgrey", fill=NA),
  #         strip.background = element_blank(),
  #         axis.line = element_line(),
  #         axis.text.x = element_text(angle = 45,
  #                                    hjust = 1),
  #         legend.position = "right",
  #         legend.key = element_blank())
  # 
  # g_daycent_ch4_byclimate
  
  g_daycent_ch4_byclimate <- model_component_means_daycent_climate_piv[
    model_component_means_daycent_climate_piv$source %in% c("CH4","SoilTemp",
                                                            "WFPS_0cm","WFPS_2cm",
                                                            "WFPS_5cm") &
      model_component_means_daycent_climate_piv$Climate_Scenario != 1,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key Daycent CH4 Model Components") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_daycent_ch4_byclimate
  
  ggsave(filename=paste0(these_results_folder,"/pub_apsim_n2o_expl_byclimate.jpg"),
         plot=g_apsim_n2o_byclimate, width=18, height=20, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_daycent_n2o_expl_byclimate.jpg"),
         plot=g_daycent_n2o_byclimate, width=18, height=20, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_daycent_ch4_expl_byclimate.jpg"),
         plot=g_daycent_ch4_byclimate, width=18, height=20, dpi=300)
  
  
  ## by climate scenario for specific management ------------------------
  
  ### RF group ---------
  
  #### KBS ----------
  
  rf_font_size <- 40
  ylim_kbs_st <- c(-75,375)
  ylim_kbs_sm <- c(-600,0)
  
  ### APSIM
  
  g_kbs_apsim_n2o_byclimate_RFgrp_N2O <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "N2O_profile_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    ggtitle("KBS: APSIM") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-125,110),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "N2O 0-200 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_N2O
  
  g_kbs_apsim_n2o_byclimate_RFgrp_NO3 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "NO3_0to60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-200,620),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "NO3 0-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_NO3
  
  
  g_kbs_apsim_n2o_byclimate_RFgrp_ST20 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SoilT_20cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 0-20 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_ST20
  
  g_kbs_apsim_n2o_byclimate_RFgrp_ST40 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SoilT_40cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 20-40 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_ST40
  
  g_kbs_apsim_n2o_byclimate_RFgrp_ST60 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SoilT_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 40-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_ST60
  
  
  g_kbs_apsim_n2o_byclimate_RFgrp_SW20 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SW_20cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 0-20 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_SW20
  
  
  g_kbs_apsim_n2o_byclimate_RFgrp_SW40 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SW_40cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 20-40 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_SW40
  
  g_kbs_apsim_n2o_byclimate_RFgrp_SW60 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SW_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 40-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          # axis.text.x = element_text(angle = 45,
          #                            hjust = 1),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_apsim_n2o_byclimate_RFgrp_SW60
  
  ### Daycent 
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_N2O <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "N2O_profile_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    ggtitle("KBS: Daycent") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-125,110),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "N2O 0-200 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_N2O
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_NO3 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "NO3_0to60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-200,620),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "NO3 0-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_NO3
  
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_ST20 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SoilT_20cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 10-20 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_ST20
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_ST40 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SoilT_40cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 20-40 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_ST40
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_ST60 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "SoilT_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 40-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_ST60
  
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS20 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "WFPS_20cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS20
  
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS40 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "WFPS_40cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 20-40 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS40
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS60 <- kbs_model_component_means_byclimate_RFgrp_piv[
    kbs_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      kbs_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      kbs_model_component_means_byclimate_RFgrp_piv$source == "WFPS_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_kbs_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 40-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          # axis.text.x = element_text(angle = 45,
          #                            hjust = 1),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS60
  
  #### group all plots into one figure
  
  gg_kbs_n2o_byclimate <- ggarrange(
    g_kbs_apsim_n2o_byclimate_RFgrp_N2O,
    g_kbs_Daycent_n2o_byclimate_RFgrp_N2O,
    g_kbs_apsim_n2o_byclimate_RFgrp_NO3,
    g_kbs_Daycent_n2o_byclimate_RFgrp_NO3,
    g_kbs_apsim_n2o_byclimate_RFgrp_ST20,
    g_kbs_Daycent_n2o_byclimate_RFgrp_ST20,
    g_kbs_apsim_n2o_byclimate_RFgrp_ST40,
    g_kbs_Daycent_n2o_byclimate_RFgrp_ST40,
    g_kbs_apsim_n2o_byclimate_RFgrp_ST60,
    g_kbs_Daycent_n2o_byclimate_RFgrp_ST60,
    g_kbs_apsim_n2o_byclimate_RFgrp_SW20,
    g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS20,
    g_kbs_apsim_n2o_byclimate_RFgrp_SW40,
    g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS40,
    g_kbs_apsim_n2o_byclimate_RFgrp_SW60,
    g_kbs_Daycent_n2o_byclimate_RFgrp_WFPS60,
    ncol=2,
    nrow=8,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline Climate (%)", 
                                     rot = 90, vjust = 1,
                                     size = rf_font_size+10, family = "serif"))
  
  gg_kbs_n2o_byclimate
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_n2o_expl_byclimate.jpg"),
         plot=gg_kbs_n2o_byclimate, width=30, height=40, dpi=300)
  
  
  #### LRF ----------
  
  ylim_lrf_st <- c(-240,150)
  ylim_lrf_sm <- c(-4100,2200)
  
  ### APSIM
  
  g_lrf_apsim_n2o_byclimate_RFgrp_N2O <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "N2O_profile_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    ggtitle("LRF: APSIM") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-310,400),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "N2O 0-200 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_N2O
  
  g_lrf_apsim_n2o_byclimate_RFgrp_NO3 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "NO3_0to60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-2200,0),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "NO3 0-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_NO3
  
  
  g_lrf_apsim_n2o_byclimate_RFgrp_ST15 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SoilT_15cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 5-15 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_ST15
  
  g_lrf_apsim_n2o_byclimate_RFgrp_ST35 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SoilT_35cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 15-35 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_ST35
  
  g_lrf_apsim_n2o_byclimate_RFgrp_ST60 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SoilT_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 35-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_ST60
  
  
  g_lrf_apsim_n2o_byclimate_RFgrp_SW15 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SW_15cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 5-15 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_SW15
  
  
  g_lrf_apsim_n2o_byclimate_RFgrp_SW35 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SW_35cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 15-35 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_SW35
  
  g_lrf_apsim_n2o_byclimate_RFgrp_SW60 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "APSIM" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SW_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "SW 35-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          # axis.text.x = element_text(angle = 45,
          #                            hjust = 1),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_apsim_n2o_byclimate_RFgrp_SW60
  
  ### Daycent 
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_N2O <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "N2O_profile_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    ggtitle("LRF: Daycent") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-310,400),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "N2O 0-200 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_N2O
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_NO3 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "NO3_0to60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = c(-2200,0),
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "NO3 0-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_NO3
  
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_ST20 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SoilT_20cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 10-20 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_ST20
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_ST45 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SoilT_45cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 30-45 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_ST45
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_ST60 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "SoilT_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_st,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "ST 45-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_ST60
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS20 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "WFPS_20cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 10-20 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS20
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS45 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "WFPS_45cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 30-45 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          #axis.ticks.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS45
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS60 <- lrf_model_component_means_byclimate_RFgrp_piv[
    lrf_model_component_means_byclimate_RFgrp_piv$Model == "Daycent" &
      lrf_model_component_means_byclimate_RFgrp_piv$Climate_Scenario != 1 &
      lrf_model_component_means_byclimate_RFgrp_piv$source == "WFPS_60cm_pctchg",] %>%
    ggplot(aes(x=pub_climate_scenario, y=vals, fill=Scenario_Abbrev)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(labels=c("RF 5%","RF 15%","RF 25%","RF 35%"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Scenario") +
    scale_y_continuous(limits = ylim_lrf_sm,
                       sec.axis = sec_axis(trans = ~ .x,
                                           name = "WFPS 45-60 cm")) +
    theme_classic(base_family = "serif", base_size = rf_font_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          # axis.text.x = element_text(angle = 45,
          #                            hjust = 1),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.left = element_blank(),
          axis.text.y.right = element_blank(),
          legend.position = "none",
          legend.key = element_blank(),
          legend.text = element_text(size=rf_font_size+10))
  
  g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS60
  
  #### group all plots into one figure
  
  gg_lrf_n2o_byclimate <- ggarrange(
    g_lrf_apsim_n2o_byclimate_RFgrp_N2O,
    g_lrf_Daycent_n2o_byclimate_RFgrp_N2O,
    g_lrf_apsim_n2o_byclimate_RFgrp_NO3,
    g_lrf_Daycent_n2o_byclimate_RFgrp_NO3,
    g_lrf_apsim_n2o_byclimate_RFgrp_ST15,
    g_lrf_Daycent_n2o_byclimate_RFgrp_ST20,
    g_lrf_apsim_n2o_byclimate_RFgrp_ST35,
    g_lrf_Daycent_n2o_byclimate_RFgrp_ST45,
    g_lrf_apsim_n2o_byclimate_RFgrp_ST60,
    g_lrf_Daycent_n2o_byclimate_RFgrp_ST60,
    g_lrf_apsim_n2o_byclimate_RFgrp_SW15,
    g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS20,
    g_lrf_apsim_n2o_byclimate_RFgrp_SW35,
    g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS45,
    g_lrf_apsim_n2o_byclimate_RFgrp_SW60,
    g_lrf_Daycent_n2o_byclimate_RFgrp_WFPS60,
    ncol=2,
    nrow=8,
    widths=4,
    heights=1,
    #font.label=c(family="serif"),
    common.legend=TRUE) %>%
    annotate_figure(left = text_grob("Percent Change from Baseline Climate (%)", 
                                     rot = 90, vjust = 1,
                                     size = rf_font_size+10, family = "serif"))
  
  gg_lrf_n2o_byclimate
  
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_n2o_expl_byclimate.jpg"),
         plot=gg_lrf_n2o_byclimate, width=30, height=40, dpi=300)
  
  
  
  #### end rf -----------------------------------------------------------------
  
  
  
  ### CR
  
  
  
  g_apsim_n2o_byclimate_CR <- kbs_model_component_means_byclimate_CR_piv[
    kbs_model_component_means_byclimate_CR_piv$source %in% c("N2O_0to60cm_pctchg") &
      kbs_model_component_means_byclimate_CR_piv$Climate_Scenario != 1,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key APSIM N2O Model Components", "Scenario: CR") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    theme_classic(base_family = "serif", base_size = 40) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_apsim_n2o_byclimate_CR
  
  
  g_daycent_n2o_byclimate_CR <- model_component_means_daycent_climate_CR_piv[
    model_component_means_daycent_climate_CR_piv$source %in% c("N2O","NO3","SoilTemp",
                                                               "WFPS_10cm","WFPS_20cm",
                                                               "WFPS_40cm") &
      model_component_means_daycent_climate_CR_piv$Climate_Scenario != 1,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key Daycent N2O Model Components","Scenario: CR") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 35) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_daycent_n2o_byclimate_CR
  
  g_daycent_ch4_byclimate_CR <- model_component_means_daycent_climate_CR_piv[
    model_component_means_daycent_climate_CR_piv$source %in% c("CH4","SoilTemp",
                                                               "WFPS_0cm","WFPS_2cm",
                                                               "WFPS_5cm") &
      model_component_means_daycent_climate_CR_piv$Climate_Scenario != 1,] %>%
    ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
    geom_col(position="dodge",colour=NA) +
    ylab("Percent Change from Baseline Climate (%)") +
    xlab("") +
    ggtitle("Key Daycent CH4 Model Components") +
    scale_fill_manual(labels=c("GFDL Low","GFDL High","UKESM Low","UKESM High"),
                      values=c(GFDL_L_color,GFDL_H_color,UKESM_L_color,
                               UKESM_H_color),
                      name="Climate Scenario") +
    facet_grid(source~site_name) +
    theme_classic(base_family = "serif", base_size = 40) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_daycent_ch4_byclimate_CR
  
  
  ggsave(filename=paste0(these_results_folder,"/pub_apsim_n2o_expl_byclimate_CR.jpg"),
         plot=g_apsim_n2o_byclimate_CR, width=18, height=20, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_daycent_n2o_expl_byclimate_CR.jpg"),
         plot=g_daycent_n2o_byclimate_CR, width=18, height=20, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_daycent_ch4_expl_byclimate_CR.jpg"),
         plot=g_daycent_ch4_byclimate_CR, width=18, height=20, dpi=300)
  
  
  
}) # end suppressMessages

