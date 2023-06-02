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
  
print(paste0("Starting 10_Model_Ensemble_results-combined_scenarios_and_sites.R"))

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)


  #*************************************************************

  
    pub_comb_results_path <- paste0("Comb_results_",end_fut_period_year,"/")

    ## Import summarized data -------------------------------------------------

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

#*************************************************************

## GWP bar charts ----------------------------------------------------------

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

ggsave(filename="Comb_results_2050/pub_all_GWP.jpg",
       plot=g_gwp, width=18, height=14, dpi=300)

## GWP components  ------------------------------------------------------------

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

ggsave(filename="Comb_results_2050/pub_all_n2oe.jpg",
       plot=g_n2oe, width=18, height=14, dpi=300)
ggsave(filename="Comb_results_2050/pub_all_ch4e.jpg",
       plot=g_ch4e, width=18, height=14, dpi=300)
ggsave(filename="Comb_results_2050/pub_all_soce.jpg",
       plot=g_soce, width=18, height=14, dpi=300)

#*******************************************************************

## GWP by source -----------------------------------------------------------


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

ggsave(filename="Comb_results_2050/pub_all_GWP_by_source.jpg",
       plot=g_gwp_source, width=18, height=14, dpi=300)

}) # end suppressMessages

