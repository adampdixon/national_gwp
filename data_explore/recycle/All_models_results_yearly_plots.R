#######################################
# File: "All_Models_Results_yearly_plots.R"
# Author: "Adam Dixon"
# Date: "June 2024"
# Description: This script generates a 5 across x 6 wide set of line graphs for each crop and scenario, showing model
# outputs 1850-2050. It is written to be run as the final set of operations after models have been run for a specific county.
#
#######################################
# do a x axis with time
# then do an average for all counties, or plot all lines

# version 2 is to plot both climate scenarios on same plot



if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}

output_figs<-national_figs

library(dplyr)
library(ggplot2)
library(reshape2) 
library(gridExtra)
library(grid)
library(data.table)

# this script has get model table functions
source(file.path(master_path, 'data_explore', 'get_model_tables.R'), local = TRUE)



# line plots of crops and scenarios
# 
df<-get_all_models_df(crops_to_get = c('Maize', 'Rotation'))
  
  # nrow(crop_scenario_df)
  # table(crop_scenario_df$GEOID)
  
  # TODO add a smoother?
  
  # Plot 5 x 6
  gfg_plot <- function(df, result){
    
    # new df
    df2<-select(df, year, climate_scen, eval(paste0(result, '_Mean')), eval(paste0(result, '_SD')))
    
    colnames(df2)<-c('year', 'climate_scen', 'mean_', 'sd_')
    
    # spline_int <- as.data.frame(spline(df2$year, df2$mean_))
    # head(spline_int)
    # 
    # x<-spline_int$x
    # y<-spline_int$y
    # 
    # plot(x = x, y =y, cex = .1)
    # lines(x = x, y = y)
    
    df3<-df2%>%mutate(lower = mean_ - sd_,
             upper = mean_ + sd_)
    
    p<-ggplot(df3, aes(x = year,
                      y = mean_,
                      ymin = lower,
                      ymax = upper,
                      fill=climate_scen, linetype=climate_scen, linecolor=climate_scen)) +
      geom_ribbon(alpha=0.25) +
      geom_line(show.legend = FALSE, color = 'black') +
      geom_vline(xintercept=2022, color = 'gray20', linetype="twodash") +
      scale_x_continuous(breaks = seq(1850, 2050, by = 10)) +
      xlab('') +
      ylab(result) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    # p<-ggplot(df, aes(x = year,
    #                   y = get(result),
    #                   color = GEOID)) +
    #   geom_line(show.legend = FALSE) +
    #   stat_summary(geom="line", fun = "mean", color="black", linewidth=.5) +
    #   geom_vline(xintercept=2022, color = 'gray20', linetype="dashed") +
    #   scale_x_continuous(breaks = seq(1850, 2050, by = 10)) +
    #   xlab('') +
    #   ylab(result) +
    #   theme_classic() +
    #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    return(p)
  }
}
  
  print('plotting')
  
  # MaizeYld_Mgha, SOC_Mgha, N2OEmissions_ghayr, CH4Emissions_ghayr
  
  s1<-filter(crop_scenario_df, scenario == 1)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))

  s2<-filter(crop_scenario_df, scenario == 2)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s3<-filter(crop_scenario_df, scenario == 3)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s4<-filter(crop_scenario_df, scenario == 4)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s5<-filter(crop_scenario_df, scenario == 5)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s6<-filter(crop_scenario_df, scenario == 6)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  
  # s1<-filter(crop_scenario_df, Scenario == 1)    
  # s2<-filter(crop_scenario_df, Scenario == 2)
  # s3<-filter(crop_scenario_df, Scenario == 3)
  # s4<-filter(crop_scenario_df, Scenario == 4)
  # s5<-filter(crop_scenario_df, Scenario == 5)
  # s6<-filter(crop_scenario_df, Scenario == 6)
  
  scen_1_yield<-gfg_plot(df = s1, result = paste0(cr, 'Yld_Mgha'))
  scen_2_yield<-gfg_plot(df = s2, result = paste0(cr, 'Yld_Mgha'))
  scen_3_yield<-gfg_plot(df = s3, result = paste0(cr, 'Yld_Mgha'))
  scen_4_yield<-gfg_plot(df = s4, result = paste0(cr, 'Yld_Mgha'))
  scen_5_yield<-gfg_plot(df = s5, result = paste0(cr, 'Yld_Mgha'))
  scen_6_yield<-gfg_plot(df = s6, result = paste0(cr, 'Yld_Mgha'))
  
  scen_1_SOC<-gfg_plot(df = s1, result = 'SOC_Mgha')
  scen_2_SOC<-gfg_plot(df = s2, result = 'SOC_Mgha')
  scen_3_SOC<-gfg_plot(df = s3, result = 'SOC_Mgha')
  scen_4_SOC<-gfg_plot(df = s4, result = 'SOC_Mgha')
  scen_5_SOC<-gfg_plot(df = s5, result = 'SOC_Mgha')
  scen_6_SOC<-gfg_plot(df = s6, result = 'SOC_Mgha')
  
  scen_1_N2O<-gfg_plot(df = s1, result = 'N2OEmissions_ghayr')
  scen_2_N2O<-gfg_plot(df = s2, result = 'N2OEmissions_ghayr')
  scen_3_N2O<-gfg_plot(df = s3, result = 'N2OEmissions_ghayr')
  scen_4_N2O<-gfg_plot(df = s4, result = 'N2OEmissions_ghayr')
  scen_5_N2O<-gfg_plot(df = s5, result = 'N2OEmissions_ghayr')
  scen_6_N2O<-gfg_plot(df = s6, result = 'N2OEmissions_ghayr')
  
  scen_1_CH4<-gfg_plot(df = s1, result = 'CH4Emissions_ghayr')
  scen_2_CH4<-gfg_plot(df = s2, result = 'CH4Emissions_ghayr')
  scen_3_CH4<-gfg_plot(df = s3, result = 'CH4Emissions_ghayr')
  scen_4_CH4<-gfg_plot(df = s4, result = 'CH4Emissions_ghayr')
  scen_5_CH4<-gfg_plot(df = s5, result = 'CH4Emissions_ghayr')
  scen_6_CH4<-gfg_plot(df = s6, result = 'CH4Emissions_ghayr')
  
  scen_1_CO2<-gfg_plot(df = s1, result = 'CO2resp_ghayr')
  scen_2_CO2<-gfg_plot(df = s2, result = 'CO2resp_ghayr')
  scen_3_CO2<-gfg_plot(df = s3, result = 'CO2resp_ghayr')
  scen_4_CO2<-gfg_plot(df = s4, result = 'CO2resp_ghayr')
  scen_5_CO2<-gfg_plot(df = s5, result = 'CO2resp_ghayr')
  scen_6_CO2<-gfg_plot(df = s6, result = 'CO2resp_ghayr')
  
  
  # if (cl==1){
  #   title<-paste0(cr, " Daycent Low Change Climate Scenario")
  # }
  # if (cl==2){
  #   title<-paste0(cr, " Daycent High Change Climate Scenario")
  # }
  
  title<-paste0(cr, " Daycent high/low radiative forcing climate scenarios")
  # }
  
  
  # out<-arrangeGrob(scen_1_yield, scen_1_SOC, scen_1_N2O, scen_1_CH4,
  #                  scen_2_yield, scen_2_SOC, scen_2_N2O, scen_2_CH4,
  #                  scen_3_yield, scen_3_SOC, scen_3_N2O, scen_3_CH4,
  #                  scen_4_yield, scen_4_SOC, scen_4_N2O, scen_4_CH4,
  #                  scen_5_yield, scen_5_SOC, scen_5_N2O, scen_5_CH4,
  #                  scen_6_yield, scen_6_SOC, scen_6_N2O, scen_6_CH4,
  #                  ncol = 4, nrow = 6,
  #                  top = textGrob(title,gp=gpar(fontsize=20,font=3)))
  
  left_justify<-.025
  
  out<-grid.arrange(arrangeGrob(scen_1_yield, scen_1_SOC, scen_1_N2O, scen_1_CH4, scen_1_CO2, top = grid::textGrob("Monocropping", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_2_yield, scen_2_SOC, scen_2_N2O, scen_2_CH4, scen_2_CO2, top = grid::textGrob("No-till", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_3_yield, scen_3_SOC, scen_3_N2O, scen_3_CH4, scen_3_CO2, top = grid::textGrob("Cover Crop Mix", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_4_yield, scen_4_SOC, scen_4_N2O, scen_4_CH4, scen_4_CO2, top = grid::textGrob("Cover Crop Cereal", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_5_yield, scen_5_SOC, scen_5_N2O, scen_5_CH4, scen_5_CO2, top = grid::textGrob("Cover Crop Legume", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_6_yield, scen_6_SOC, scen_6_N2O, scen_6_CH4, scen_6_CO2, top = grid::textGrob("No-till and Cover Crop", x = left_justify, hjust = 0), ncol = 5),
                    nrow = 6,
                    top = textGrob(title,gp=gpar(fontsize=20,font=3)))
  
  
  
  crop_out<-file.path(output_figs, paste0('both_clim_scen_', cr,  "_results_", date, ".png"))
  
  ggsave(file = crop_out, plot=out, dpi=300, width = 20, height = 16)
  
  print('done')
  
# } # end of crop loop









