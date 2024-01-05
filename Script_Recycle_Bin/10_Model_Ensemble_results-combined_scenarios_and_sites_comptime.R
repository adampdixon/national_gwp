#*************************************************************
# File: 10_Model_Ensemble_Results-combined_scenarios_and_sites
# Author: Ellen Maas
# Date: June 2022
# Description: Creates graphs for publication which combine
# all scenarios and both sites into a single figure.
#
# ****NOTE: This version calculates % change in model components
#            over the future period.
#*************************************************************
# Calls:
# f_model_coef
#*************************************************************
# Audit Log
# June 2022: Created script for GWP graphs.
# July 2022: Added explanatory graphs with model components.
#*************************************************************

suppressMessages({
  
  print(paste0("Starting 10_Model_Ensemble_results-combined_scenarios_and_sites3.R"))
  
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  
  
  #*************************************************************
  
  
  pub_comb_results_path <- paste0("Comb_results_",end_fut_period_year,"/")
  
  # Import GWP component data --------------------------------------
  
  kbs_summary_output <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/summary_output_final.csv")) %>%
    mutate(site_name="KBS")
  kbs_scenario_means <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/scenario_means.csv")) %>%
    mutate(site_name="KBS")
  kbs_annual_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/annual_results.csv")) %>%
    mutate(site_name="KBS")
  kbs_mean_annual_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                             "/mean_annual_results.csv")) %>%
    mutate(site_name="KBS")
  kbs_daily_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                       "/daily_results.csv")) %>%
    mutate(site_name="KBS")
  kbs_mean_daily_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                            "/mean_daily_results.csv")) %>%
    mutate(site_name="KBS")
  
  lrf_summary_output <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/summary_output_final.csv")) %>%
    mutate(site_name="LRF")
  lrf_scenario_means <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/scenario_means.csv")) %>%
    mutate(site_name="LRF")
  lrf_annual_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/annual_results.csv")) %>%
    mutate(site_name="LRF")
  lrf_mean_annual_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                             "/mean_annual_results.csv")) %>%
    mutate(site_name="LRF")
  lrf_daily_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                       "/daily_results.csv")) %>%
    mutate(site_name="LRF")
  lrf_mean_daily_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                            "/mean_daily_results.csv")) %>%
    mutate(site_name="LRF")
  
  
  ## Rearrange data for output -----------------------------------------------
  
  
  kbs_summary_output_piv <- pivot_longer(kbs_summary_output,
                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals")
  kbs_scenario_means_piv <- pivot_longer(kbs_scenario_means,
                                         c(-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
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
                                         names_to="source",values_to="vals")
  lrf_scenario_means_piv <- pivot_longer(lrf_scenario_means,
                                         c(-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
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
  
  summary_output_piv <- left_join(rbind(kbs_summary_output_piv,lrf_summary_output_piv),
                                  unique(scenario_df[,c("climate_scenario_num",
                                                        "climate_desc")]),
                                  by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
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
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
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
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
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
  write.csv(gwp_scenario_means, file=paste0("Comb_results_",end_fut_period_year,"/gwp_scenario_means.csv"),
            row.names=FALSE)
  write.csv(gwp_summary_output, file=paste0("Comb_results_",end_fut_period_year,"/gwp_summary_output.csv"),
            row.names=FALSE)
  
  # Import model explanatory components data --------------------------------

  kbs_model_components <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/Summary_future_output.csv")) %>%
    # add % change in components over time
    mutate(site_name="KBS",
           SW_20cm_pctchg = SW_20cm_change/SW_20cm_first*100,
           SW_40cm_pctchg = SW_40cm_change/SW_40cm_first*100,
           SW_60cm_pctchg = SW_60cm_change/SW_60cm_first*100,
           DW_20cm_pctchg = DW_20cm_change/DW_20cm_first*100,
           DW_40cm_pctchg = DW_40cm_change/DW_40cm_first*100,
           DW_60cm_pctchg = DW_60cm_change/DW_60cm_first*100,
           DW_0to60cm_pctchg = DW_0to60cm_change/DW_0to60cm_first*100,
           SW_25cm_pctchg = SW_25cm_change/SW_25cm_first*100,
           DW_25cm_pctchg = DW_25cm_change/DW_25cm_first*100,
           SoilT_20cm_pctchg = SoilT_20cm_change/SoilT_20cm_first*100,
           SoilT_40cm_pctchg = SoilT_40cm_change/SoilT_40cm_first*100,
           SoilT_60cm_pctchg = SoilT_60cm_change/SoilT_60cm_first*100,
           SoilT_25cm_pctchg = SoilT_25cm_change/SoilT_25cm_first*100,
           NO3_20cm_pctchg = NO3_20cm_change/NO3_20cm_first*100,
           NO3_40cm_pctchg = NO3_40cm_change/NO3_40cm_first*100,
           NO3_60cm_pctchg = NO3_60cm_change/NO3_60cm_first*100,
           NO3_0to60cm_pctchg = NO3_0to60cm_change/NO3_0to60cm_first*100,
           N2O_20cm_pctchg = N2O_20cm_change/N2O_20cm_first*100,
           N2O_40cm_pctchg = N2O_40cm_change/N2O_40cm_first*100,
           N2O_60cm_pctchg = N2O_60cm_change/N2O_60cm_first*100,
           N2O_0to60cm_pctchg = N2O_0to60cm_change/N2O_0to60cm_first*100,
           N2O_profile_pctchg = N2O_profile_change/N2O_profile_first*100,
           BC_25cm_pctchg = BC_25cm_change/BC_25cm_first*100,
           BN_25cm_pctchg = BN_25cm_change/BN_25cm_first*100,
           HC_25cm_pctchg = HC_25cm_change/HC_25cm_first*100,
           HN_25cm_pctchg = HN_25cm_change/HN_25cm_first*100,
           CinB_25cm_pctchg = CinB_25cm_change/CinB_25cm_first*100,
           CinH_25cm_pctchg = CinH_25cm_change/CinH_25cm_first*100,
           CinBtoH_25cm_pctchg = CinBtoH_25cm_change/CinBtoH_25cm_first*100,
           SOC_25cm_pctchg = SOC_25cm_change/SOC_25cm_first*100,
           DW_2cm_pctchg = DW_2cm_change/DW_2cm_first*100,
           DW_5cm_pctchg = DW_5cm_change/DW_5cm_first*100,
           DW_10cm_pctchg = DW_10cm_change/DW_10cm_first*100,
           WFPS_2cm_pctchg = WFPS_2cm_change/WFPS_2cm_first*100,
           WFPS_5cm_pctchg = WFPS_5cm_change/WFPS_5cm_first*100,
           WFPS_10cm_pctchg = WFPS_10cm_change/WFPS_10cm_first*100,
           WFPS_20cm_pctchg = WFPS_20cm_change/WFPS_20cm_first*100,
           WFPS_40cm_pctchg = WFPS_40cm_change/WFPS_40cm_first*100,
           WFPS_60cm_pctchg = WFPS_60cm_change/WFPS_60cm_first*100,
           SoilT_2cm_pctchg = SoilT_2cm_change/SoilT_2cm_first*100,
           SoilT_5cm_pctchg = SoilT_5cm_change/SoilT_5cm_first*100,
           SoilT_10cm_pctchg = SoilT_10cm_change/SoilT_10cm_first*100,
           SoilT_15cm_pctchg = SoilT_15cm_change/SoilT_15cm_first*100,
           NO3_2cm_pctchg = NO3_2cm_change/NO3_2cm_first*100,
           NO3_5cm_pctchg = NO3_5cm_change/NO3_5cm_first*100,
           CH4_pctchg = CH4_change/CH4_first*100,
           CI_pctchg = CI_change/CI_first*100)
    # # add % change in component compared to the baseline climate
    # group_by(Model,Mgmt_Scenario) %>%
    # mutate(SW_20cm_climate=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
    #        SW_40cm_climate=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
    #        SW_60cm_climate=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
    #        NO3_20cm_climate=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
    #        N2O_20cm_climate=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
    #        SoilTemp_20cm_climate=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1)) %>%
    # ungroup()

  
  #*************************************************************
  
  # GWP stats ---------------------------------------------------------------
  
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
  write.csv(summary_ghg_stats, file=paste0("Comb_results_",end_fut_period_year,"/summary_ghg_stats_min_max.csv"),
            row.names=FALSE)
  
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
  write.csv(diffs_from_controls, file=paste0("Comb_results_",end_fut_period_year,"/diffs_from_controls.csv"),
            row.names=FALSE)
  
  ### calculate min and max diffs from control
  
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
                                            max_diff_gwp)
  colnames(summary_ghg_stats_diff_from_ctrl) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr","max_co2e_n2o","max_co2e_n2o_scen_abbr",
                                                  "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr","max_co2e_ch4","max_co2e_ch4_scen_abbr",
                                                  "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr","max_co2e_soc","max_co2e_soc_scen_abbr",
                                                  "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr","max_co2e_gwp",
                                                  "scenario_abbrev","site_name","Climate_Scenario","climate_desc")
  
  ### write data frame
  write.csv(summary_ghg_stats_diff_from_ctrl, file=paste0("Comb_results_",end_fut_period_year,"/summary_ghg_stats_diff_from_ctrl.csv"),
            row.names=FALSE)
  
  
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
  write.csv(diffs_from_base, file=paste0("Comb_results_",end_fut_period_year,"/diffs_from_base_climate.csv"),
            row.names=FALSE)
  
  ### calculate min and max diffs from baseline
  
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
  
  ##### add blank BC rows: no data because APSIM doesn't do methane
  min_diff_co2e_ch4_clim <- min_diff_co2e_ch4_clim %>%
    rbind(data.frame(site_name=c("KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF"),
                     climate_desc.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     Climate_Scenario.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     scenario_abbrev=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                       "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                     diff_co2e_ch4=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    )
  
  max_diff_co2e_ch4_clim <- max_diff_co2e_ch4_clim %>%
    rbind(data.frame(site_name=c("KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF"),
                     climate_desc.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     Climate_Scenario.y=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     scenario_abbrev=c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                       "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                     diff_co2e_ch4=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    )
  
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
                                                    max_diff_gwp_clim)
  
  colnames(summary_ghg_stats_diff_from_ctrl) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr","max_co2e_n2o","max_co2e_n2o_scen_abbr",
                                                  "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr","max_co2e_ch4","max_co2e_ch4_scen_abbr",
                                                  "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr","max_co2e_soc","max_co2e_soc_scen_abbr",
                                                  "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr","max_co2e_gwp",
                                                  "scenario_abbrev","site_name","Climate_Scenario","climate_desc")
  
  ### write data frame
  write.csv(summary_ghg_stats_diff_from_base_climate, 
            file=paste0("Comb_results_",end_fut_period_year,"/summary_ghg_stats_diff_from_base_climate.csv"),
            row.names=FALSE)
  
  
  #*************************************************************
  
  # GWP bar charts ----------------------------------------------------------
  
  g_gwp <- summary_output_piv[summary_output_piv$source == "GWP",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_GWP), color= "black",
             fill=NA, position="dodge") +
    annotate("rect", xmin = 0.5, xmax = 5.5,
             ymin = min(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 5.5, xmax = 7.5,
             ymin = min(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 7.5, xmax = 8.5,
             ymin = min(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 8.5, xmax = 10.5,
             ymin = min(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 10.5, xmax = 14.5,
             ymin = min(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 14.5, xmax = 20.5,
             ymin = min(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"GWP"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Global Warming Potential by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                      values=cbPalette9[c(8,2,6,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "right",
          legend.key = element_blank())
  
  g_gwp
  
  ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_all_GWP.jpg"),
         plot=g_gwp, width=18, height=14, dpi=300)
  
  # GWP components  ------------------------------------------------------------
  
  g_n2oe <- summary_output_piv[summary_output_piv$source == "CO2e_N2O",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_N2O), color= "black", 
             fill=NA, position="dodge") + 
    annotate("rect", xmin = 0.5, xmax = 5.5,
             ymin = 0,
             ymax = max(gwp_summary_output[,"CO2e_N2O"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 5.5, xmax = 7.5,
             ymin = 0,
             ymax = max(gwp_summary_output[,"CO2e_N2O"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 7.5, xmax = 8.5,
             ymin = 0,
             ymax = max(gwp_summary_output[,"CO2e_N2O"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 8.5, xmax = 10.5,
             ymin = 0,
             ymax = max(gwp_summary_output[,"CO2e_N2O"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 10.5, xmax = 14.5,
             ymin = 0,
             ymax = max(gwp_summary_output[,"CO2e_N2O"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 14.5, xmax = 20.5,
             ymin = 0,
             ymax = max(gwp_summary_output[,"CO2e_N2O"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    ylab(expression('CO'[2]*'e N'[2]*'O (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Change in CO2e-N2O by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                      values=cbPalette9[c(8,2,6,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "right",
          legend.key = element_blank())
  
  g_n2oe
  
  
  g_ch4e <- summary_output_piv[summary_output_piv$source == "CO2e_CH4",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black",
             fill=NA, position="dodge") +
    annotate("rect", xmin = 0.5, xmax = 5.5,
             ymin = min(gwp_summary_output[,"CO2e_CH4"]*1.05, na.rm=T),
             ymax = 0,
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 5.5, xmax = 7.5,
             ymin = min(gwp_summary_output[,"CO2e_CH4"]*1.05, na.rm=T),
             ymax = 0,
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 7.5, xmax = 8.5,
             ymin = min(gwp_summary_output[,"CO2e_CH4"]*1.05, na.rm=T),
             ymax = 0,
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 8.5, xmax = 10.5,
             ymin = min(gwp_summary_output[,"CO2e_CH4"]*1.05, na.rm=T),
             ymax = 0,
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 10.5, xmax = 14.5,
             ymin = min(gwp_summary_output[,"CO2e_CH4"]*1.05, na.rm=T),
             ymax = 0,
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 14.5, xmax = 20.5,
             ymin = min(gwp_summary_output[,"CO2e_CH4"]*1.05, na.rm=T),
             ymax = 0,
             alpha = 0, color= "grey") +
    ylab(expression('CO'[2]*'e CH'[4]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Change in CO2e-CH4 by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                      values=cbPalette9[c(8,2,6,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "right",
          legend.key = element_blank())
  
  g_ch4e
  
  g_soce <- summary_output_piv[summary_output_piv$source == "CO2e_SOC",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_SOC), color= "black",
             fill=NA, position="dodge") +
    annotate("rect", xmin = 0.5, xmax = 5.5,
             ymin = min(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 5.5, xmax = 7.5,
             ymin = min(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 7.5, xmax = 8.5,
             ymin = min(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 8.5, xmax = 10.5,
             ymin = min(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 10.5, xmax = 14.5,
             ymin = min(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 14.5, xmax = 20.5,
             ymin = min(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             ymax = max(gwp_summary_output[,"CO2e_SOC"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    ylab(expression('CO'[2]*'e SOC (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Change in CO2e-SOC by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                      values=cbPalette9[c(8,2,6,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "right",
          legend.key = element_blank())
  
  g_soce
  
  ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_all_n2oe.jpg"),
         plot=g_n2oe, width=18, height=14, dpi=300)
  ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_all_ch4e.jpg"),
         plot=g_ch4e, width=18, height=14, dpi=300)
  ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_all_soce.jpg"),
         plot=g_soce, width=18, height=14, dpi=300)
  
  #*******************************************************************
  
  # GWP by source -----------------------------------------------------------
  
  
  g_gwp_source <- gwp_scenario_means_piv[gwp_scenario_means_piv$source %in% 
                                           c("mean_CO2e_SOC","mean_CO2e_N2O","mean_CO2e_CH4"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=source)) +
    geom_col(position="stack") +
    annotate("rect", xmin = 0.5, xmax = 5.5,
             ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 5.5, xmax = 6.5,
             ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 6.5, xmax = 7.5,
             ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 7.5, xmax = 8.5,
             ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 8.5, xmax = 12.5,
             ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +
    annotate("rect", xmin = 12.5, xmax = 18.5,
             ymin = min(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             ymax = max(gwp_scenario_means_piv[,"vals"]*1.05, na.rm=T),
             alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle("Global Warming Potential by Source by ",end_fut_period_year) +
    labs(fill = "source") +
    scale_fill_manual(labels=c("CO2e-CH4","CO2e-N2O","CO2e-SOC"),
                      values=cbPalette12[c(10,11,12)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "right",
          legend.key = element_blank())
  
  g_gwp_source
  
  ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_all_GWP_by_source.jpg"),
         plot=g_gwp_source, width=18, height=14, dpi=300)
  
  #*******************************************************************
  
  # Model components ---------------------------------------------------------
  

  ## All climate and mgmt scenarios ------------------------------------------

  ## First, calculate percent changes for each model component over all models
  ## and climate and management scenarios. This will indicate the sensitivity of
  ## each model component to management and climate shifts over time.
  
  ### APSIM
  
  kbs_model_components_apsim
  
  kbs_model_component_means_apsim_climate <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
                                                             SW_60cm_change,SW_25cm_change,
                                                             DW_20cm_change,
                                                             DW_40cm_change,DW_60cm_change,
                                                             DW_0to60cm_change,DW_25cm_change,
                                                             SoilT_20cm_change,SoilT_40cm_change,
                                                             SoilT_60cm_change,SoilT_25cm_change,
                                                             NO3_20cm_change,NO3_40cm_change,
                                                             NO3_60cm_change,NO3_0to60cm_change,
                                                             N2O_20cm_change,N2O_40cm_change,
                                                             N2O_60cm_change,N2O_0to60cm_change,
                                                             N2O_profile_change,
                                                             BC_25cm_change,BN_25cm_change,
                                                             HC_25cm_change,HN_25cm_change,
                                                             CinB_25cm_change,CinH_25cm_change,
                                                             CinBtoH_25cm_change,SOC_25cm_change) 
                                                       ~ Model+Climate_Scenario+site_name, 
                                                        data=kbs_model_components[kbs_model_components$Model=="APSIM",], 
                                                        FUN=mean,
                                                        na.action=na.omit) %>%
    mutate(SW_20cm=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           NO3=round((NO3_change-NO3_change[Climate_Scenario==1])/NO3_change[Climate_Scenario==1]*100,1),
           N2O=round((N2O_change-N2O_change[Climate_Scenario==1])/N2O_change[Climate_Scenario==1]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Climate_Scenario==1])/SoilT_change[Climate_Scenario==1]*100,1))
  
  kbs_model_component_means_apsim_mgmt <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
                                                             SW_60cm_change,
                                                             NO3_change,N2O_change,
                                                             SoilT_change) ~ Model+Scenario_Abbrev+site_name, 
                                                       data=kbs_model_components[kbs_model_components$Model=="APSIM",], 
                                                       FUN=mean,
                                                       na.action=na.omit) %>%
    mutate(SW_20cm=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="CR"])/SW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_40cm=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="CR"])/SW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_60cm=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="CR"])/SW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3=round((NO3_change-NO3_change[Scenario_Abbrev=="CR"])/NO3_change[Scenario_Abbrev=="CR"]*100,1),
           N2O=round((N2O_change-N2O_change[Scenario_Abbrev=="CR"])/N2O_change[Scenario_Abbrev=="CR"]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Scenario_Abbrev=="CR"])/SoilT_change[Scenario_Abbrev=="CR"]*100,1))

  
    ## Daycent
  
  kbs_model_component_means_daycent_climate <- aggregate(cbind(WFPS_10cm_change,
                                                                WFPS_20cm_change,WFPS_40cm_change,
                                                                NO3_change,N2O_change,
                                                                WFPS_0cm_change,WFPS_2cm_change,
                                                                WFPS_5cm_change,SoilT_change,
                                                                CH4_change,CI_change) ~ Model+Climate_Scenario+site_name, 
                                                          data=kbs_model_components[kbs_model_components$Model=="Daycent",], 
                                                          FUN=mean,
                                                          na.action=na.omit) %>%
    mutate(WFPS_0cm=round((WFPS_0cm_change-WFPS_0cm_change[Climate_Scenario==1])/WFPS_0cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm=round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm=round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm=round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm=round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm=round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           NO3=round((NO3_change-NO3_change[Climate_Scenario==1])/NO3_change[Climate_Scenario==1]*100,1),
           N2O=round((N2O_change-N2O_change[Climate_Scenario==1])/N2O_change[Climate_Scenario==1]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Climate_Scenario==1])/SoilT_change[Climate_Scenario==1]*100,1),
           CH4=round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI=round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )

  kbs_model_component_means_daycent_mgmt <- aggregate(cbind(WFPS_10cm_change,
                                                               WFPS_20cm_change,WFPS_40cm_change,
                                                               NO3_change,N2O_change,
                                                               WFPS_0cm_change,WFPS_2cm_change,
                                                               WFPS_5cm_change,SoilT_change,
                                                               CH4_change,CI_change) ~ Model+Scenario_Abbrev+site_name, 
                                                         data=kbs_model_components[kbs_model_components$Model=="Daycent",], 
                                                         FUN=mean,
                                                         na.action=na.omit) %>%
    mutate(WFPS_0cm=round((WFPS_0cm_change-WFPS_0cm_change[Scenario_Abbrev=="CR"])/WFPS_0cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_2cm=round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_5cm=round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_10cm=round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_20cm=round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_40cm=round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3=round((NO3_change-NO3_change[Scenario_Abbrev=="CR"])/NO3_change[Scenario_Abbrev=="CR"]*100,1),
           N2O=round((N2O_change-N2O_change[Scenario_Abbrev=="CR"])/N2O_change[Scenario_Abbrev=="CR"]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Scenario_Abbrev=="CR"])/SoilT_change[Scenario_Abbrev=="CR"]*100,1),
           CH4=round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
           CI=round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1))
  
  
  ### Combine sites for each model
  model_component_means_apsim_climate <- rbind(kbs_model_component_means_apsim_climate)
  
  model_component_means_apsim_mgmt <- rbind(kbs_model_component_means_apsim_mgmt)
  
  model_component_means_daycent_climate <- rbind(kbs_model_component_means_daycent_climate)
  
  model_component_means_daycent_mgmt <- rbind(kbs_model_component_means_daycent_mgmt)
  
  
  model_component_means_apsim_climate_piv <- pivot_longer(model_component_means_apsim_climate,
                                         c(-Model,-Climate_Scenario,-site_name),
                                         names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
  model_component_means_apsim_mgmt_piv <- pivot_longer(model_component_means_apsim_mgmt,
                                                          c(-Model,-Scenario_Abbrev,-site_name),
                                                          names_to="source",values_to="vals")
  
  model_component_means_daycent_climate_piv <- pivot_longer(model_component_means_daycent_climate,
                                                          c(-Model,-Climate_Scenario,-site_name),
                                                          names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
  model_component_means_daycent_mgmt_piv <- pivot_longer(model_component_means_daycent_mgmt,
                                                       c(-Model,-Scenario_Abbrev,-site_name),
                                                       names_to="source",values_to="vals") 


  ## Baseline climate and mgmt scenarios ------------------------------------------

  ## Now, calculate percent changes for each model component over all models
  ## for baseline climate and mgmt scenarios only.
  
  kbs_model_component_means_apsim_climate_CR <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
                                                             SW_60cm_change,
                                                             NO3_change,N2O_change,
                                                             SoilT_change) ~ Model+Climate_Scenario+site_name, 
                                                       data=kbs_model_components[kbs_model_components$Model=="APSIM" &
                                                                                   kbs_model_components$Scenario_Abbrev=="CR",], 
                                                       FUN=mean,
                                                       na.action=na.omit) %>%
    mutate(SW_20cm=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           NO3=round((NO3_change-NO3_change[Climate_Scenario==1])/NO3_change[Climate_Scenario==1]*100,1),
           N2O=round((N2O_change-N2O_change[Climate_Scenario==1])/N2O_change[Climate_Scenario==1]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Climate_Scenario==1])/SoilT_change[Climate_Scenario==1]*100,1))
  
  kbs_model_component_means_apsim_climate_NTCR <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
                                                                  SW_60cm_change,
                                                                  NO3_change,N2O_change,
                                                                  SoilT_change) ~ Model+Climate_Scenario+site_name, 
                                                            data=kbs_model_components[kbs_model_components$Model=="APSIM" &
                                                                                        kbs_model_components$Scenario_Abbrev=="NT-CR",], 
                                                            FUN=mean,
                                                            na.action=na.omit) %>%
    mutate(SW_20cm=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
           SW_40cm=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
           SW_60cm=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
           NO3=round((NO3_change-NO3_change[Climate_Scenario==1])/NO3_change[Climate_Scenario==1]*100,1),
           N2O=round((N2O_change-N2O_change[Climate_Scenario==1])/N2O_change[Climate_Scenario==1]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Climate_Scenario==1])/SoilT_change[Climate_Scenario==1]*100,1))


  kbs_model_component_means_apsim_mgmt_baseline <- aggregate(cbind(SW_20cm_change,SW_40cm_change,
                                                          SW_60cm_change,
                                                          NO3_change,N2O_change,
                                                          SoilT_change) ~ Model+Scenario_Abbrev+site_name, 
                                                    data=kbs_model_components[kbs_model_components$Model=="APSIM" &
                                                                                kbs_model_components$Climate_Scenario==1,], 
                                                    FUN=mean,
                                                    na.action=na.omit) %>%
    mutate(SW_20cm=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="CR"])/SW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_40cm=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="CR"])/SW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           SW_60cm=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="CR"])/SW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3=round((NO3_change-NO3_change[Scenario_Abbrev=="CR"])/NO3_change[Scenario_Abbrev=="CR"]*100,1),
           N2O=round((N2O_change-N2O_change[Scenario_Abbrev=="CR"])/N2O_change[Scenario_Abbrev=="CR"]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Scenario_Abbrev=="CR"])/SoilT_change[Scenario_Abbrev=="CR"]*100,1))
  
  kbs_model_component_means_daycent_climate_CR <- aggregate(cbind(WFPS_10cm_change,
                                                               WFPS_20cm_change,WFPS_40cm_change,
                                                               NO3_change,N2O_change,
                                                               WFPS_0cm_change,WFPS_2cm_change,
                                                               WFPS_5cm_change,SoilT_change,
                                                               CH4_change,CI_change) ~ Model+Climate_Scenario+site_name, 
                                                         data=kbs_model_components[kbs_model_components$Model=="Daycent" &
                                                                                     kbs_model_components$Scenario_Abbrev=="CR",], 
                                                         FUN=mean,
                                                         na.action=na.omit) %>%
    mutate(WFPS_0cm=round((WFPS_0cm_change-WFPS_0cm_change[Climate_Scenario==1])/WFPS_0cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm=round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm=round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm=round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm=round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm=round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           NO3=round((NO3_change-NO3_change[Climate_Scenario==1])/NO3_change[Climate_Scenario==1]*100,1),
           N2O=round((N2O_change-N2O_change[Climate_Scenario==1])/N2O_change[Climate_Scenario==1]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Climate_Scenario==1])/SoilT_change[Climate_Scenario==1]*100,1),
           CH4=round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI=round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )

  kbs_model_component_means_daycent_climate_NTCR <- aggregate(cbind(WFPS_10cm_change,
                                                                  WFPS_20cm_change,WFPS_40cm_change,
                                                                  NO3_change,N2O_change,
                                                                  WFPS_0cm_change,WFPS_2cm_change,
                                                                  WFPS_5cm_change,SoilT_change,
                                                                  CH4_change,CI_change) ~ Model+Climate_Scenario+site_name, 
                                                            data=kbs_model_components[kbs_model_components$Model=="Daycent" &
                                                                                        kbs_model_components$Scenario_Abbrev=="NT-CR",], 
                                                            FUN=mean,
                                                            na.action=na.omit) %>%
    mutate(WFPS_0cm=round((WFPS_0cm_change-WFPS_0cm_change[Climate_Scenario==1])/WFPS_0cm_change[Climate_Scenario==1]*100,1),
           WFPS_2cm=round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
           WFPS_5cm=round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
           WFPS_10cm=round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
           WFPS_20cm=round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
           WFPS_40cm=round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
           NO3=round((NO3_change-NO3_change[Climate_Scenario==1])/NO3_change[Climate_Scenario==1]*100,1),
           N2O=round((N2O_change-N2O_change[Climate_Scenario==1])/N2O_change[Climate_Scenario==1]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Climate_Scenario==1])/SoilT_change[Climate_Scenario==1]*100,1),
           CH4=round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
           CI=round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
    )
  
  kbs_model_component_means_daycent_mgmt_baseline <- aggregate(cbind(WFPS_10cm_change,
                                                            WFPS_20cm_change,WFPS_40cm_change,
                                                            NO3_change,N2O_change,
                                                            WFPS_0cm_change,WFPS_2cm_change,
                                                            WFPS_5cm_change,SoilT_change,
                                                            CH4_change,CI_change) ~ Model+Scenario_Abbrev+site_name, 
                                                      data=kbs_model_components[kbs_model_components$Model=="Daycent" &
                                                                                  kbs_model_components$Climate_Scenario==1,], 
                                                      FUN=mean,
                                                      na.action=na.omit) %>%
    mutate(WFPS_0cm=round((WFPS_0cm_change-WFPS_0cm_change[Scenario_Abbrev=="CR"])/WFPS_0cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_2cm=round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_5cm=round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_10cm=round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_20cm=round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
           WFPS_40cm=round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
           NO3=round((NO3_change-NO3_change[Scenario_Abbrev=="CR"])/NO3_change[Scenario_Abbrev=="CR"]*100,1),
           N2O=round((N2O_change-N2O_change[Scenario_Abbrev=="CR"])/N2O_change[Scenario_Abbrev=="CR"]*100,1),
           SoilTemp=round((SoilT_change-SoilT_change[Scenario_Abbrev=="CR"])/SoilT_change[Scenario_Abbrev=="CR"]*100,1),
           CH4=round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
           CI=round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1))
  
  ### Combine sites for each model
  model_component_means_apsim_climate_CR <- rbind(kbs_model_component_means_apsim_climate_CR)
  
  model_component_means_apsim_mgmt_baseline <- rbind(kbs_model_component_means_apsim_mgmt_baseline)
  
  model_component_means_daycent_climate_CR <- rbind(kbs_model_component_means_daycent_climate_CR)
  
  model_component_means_daycent_mgmt_baseline <- rbind(kbs_model_component_means_daycent_mgmt_baseline)
  
  
  model_component_means_apsim_climate_CR_piv <- pivot_longer(model_component_means_apsim_climate_CR,
                                                          c(-Model,-Climate_Scenario,-site_name),
                                                          names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
  model_component_means_apsim_mgmt_baseline_piv <- pivot_longer(model_component_means_apsim_mgmt_baseline,
                                                       c(-Model,-Scenario_Abbrev,-site_name),
                                                       names_to="source",values_to="vals")
  
  model_component_means_daycent_climate_CR_piv <- pivot_longer(model_component_means_daycent_climate_CR,
                                                            c(-Model,-Climate_Scenario,-site_name),
                                                            names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=c("Baseline","GFDL_Low","GFDL_High","UKESM_Low","UKESM_High")))
  
  model_component_means_daycent_mgmt_baseline_piv <- pivot_longer(model_component_means_daycent_mgmt_baseline,
                                                         c(-Model,-Scenario_Abbrev,-site_name),
                                                         names_to="source",values_to="vals") 
  
  

# Graph - by climate scenario ---------------------------------------------


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
  
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_apsim_n2o_expl_byclimate.jpg"),
         plot=g_apsim_n2o_byclimate, width=18, height=20, dpi=300)
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_daycent_n2o_expl_byclimate.jpg"),
           plot=g_daycent_n2o_byclimate, width=18, height=20, dpi=300)
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_daycent_ch4_expl_byclimate.jpg"),
           plot=g_daycent_ch4_byclimate, width=18, height=20, dpi=300)
    
  
    # Graph - by climate scenario for specific management ------------------------
    
    g_apsim_n2o_byclimate_CR <- model_component_means_apsim_climate_CR_piv[
      model_component_means_apsim_climate_CR_piv$source %in% c("N2O","NO3","SoilTemp",
                                                            "SW_20cm","SW_40cm","SW_60cm") &
        model_component_means_apsim_climate_CR_piv$Climate_Scenario != 1,] %>%
      ggplot(aes(x=climate_desc, y=vals, fill=climate_desc)) +
      geom_col(position="dodge",colour=NA) +
      ylab("Percent Change from Baseline Climate (%)") +
      xlab("") +
      ggtitle("Key APSIM N2O Model Components", "Scenario: CR") +
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
    
    
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_apsim_n2o_expl_byclimate_CR.jpg"),
           plot=g_apsim_n2o_byclimate_CR, width=18, height=20, dpi=300)
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_daycent_n2o_expl_byclimate_CR.jpg"),
           plot=g_daycent_n2o_byclimate_CR, width=18, height=20, dpi=300)
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_daycent_ch4_expl_byclimate_CR.jpg"),
           plot=g_daycent_ch4_byclimate_CR, width=18, height=20, dpi=300)
    
    # Graph - by management scenario ---------------------------------------------
    
    #### percent change in means of APSIM model components compared to the
    #### baseline mgmt scenario, through the end of the future period
    g_apsim_n2o_bymgmt <- model_component_means_apsim_mgmt_piv[
      model_component_means_apsim_mgmt_piv$source %in% c("N2O","NO3","SoilTemp",
                                                            "SW_20cm","SW_40cm","SW_60cm") &
        model_component_means_apsim_mgmt_piv$Scenario_Abbrev != "CR",] %>%
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
    
    g_apsim_n2o_bymgmt
    
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
    
    
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_apsim_n2o_expl_bymgmt.jpg"),
           plot=g_apsim_n2o_bymgmt, width=18, height=20, dpi=300)
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_daycent_n2o_expl_bymgmt.jpg"),
           plot=g_daycent_n2o_bymgmt, width=18, height=20, dpi=300)
    ggsave(filename=paste0("Comb_results_",end_fut_period_year,"/pub_daycent_ch4_expl_bymgmt.jpg"),
           plot=g_daycent_ch4_bymgmt, width=18, height=20, dpi=300)
    
    
}) # end suppressMessages

