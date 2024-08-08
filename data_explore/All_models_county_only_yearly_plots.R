#######################################
# File: "All_models_county_only_yearly_plots.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: This script generates a 5 across x 6 wide set of line graphs for each crop and scenario, showing model
# outputs 1850-2050.
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



library(dplyr)
library(ggplot2)
library(reshape2) 
library(gridExtra)
library(grid)
library(data.table)

# this script has get model table functions
source(file.path(master_path, 'data_explore', 'get_model_tables.R'), local = TRUE)


create_model_linegraphs<-function(crop){
  # c(31181, 13023, 13213, 20073, 42053, 1075))
  
  cr<-crop # should be in global environment
  
  # Function from get_model_tables.R
  county_df<-get_county_models_df(crop_to_get = cr, GEOID = county_geoid) # county_geoid in global env
  
  output_figs<-file.path(results_path, 'data_and_figs')
  
  print('plotting')
  
  # MaizeYld_Mgha, SOC_Mgha, N2OEmissions_ghayr, CH4Emissions_ghayr
  
  s1<-filter(county_df, mgmt_scenario_num == 1)
  s2<-filter(county_df, mgmt_scenario_num == 2)
  s3<-filter(county_df, mgmt_scenario_num == 3)
  s4<-filter(county_df, mgmt_scenario_num == 4)
  s5<-filter(county_df, mgmt_scenario_num == 5)
  s6<-filter(county_df, mgmt_scenario_num == 6)
  
  gfg_plot <- function(df, result){
    
    
    # remove millennial if it didn't output variable
    if (grepl("Yld_Mgha", result) | identical(result, 'N2OEmissions_ghayr') | identical(result, 'CH4Emissions_ghayr')){
      df<-df%>%
        filter(model != 'Millennial')
    }
    
    # if rotation, name the crop yield rotation for both maize/soybeans
    if(identical(cr, 'Rotation') & grepl("Yld_Mgha", result)){
      df<-df%>%
        mutate(RotationYld_Mgha = ifelse(is.na(MaizeYld_Mgha), SoybeanYld_Mgha, MaizeYld_Mgha))
      
      # check for, report, remove NAs
      n<-sum(is.na(df$RotationYld_Mgha))
      if(n>1){
        print(paste(n, 'total NAs in RotationYld_Mgha', result))
        # df<-df%>% # this is not needed or appropriate probably
        #   mutate(RotationYld_Mgha = na.locf(RotationYld_Mgha))
      }
      
    }
    

    
    
    ##############################################################################
    # SET Y AXIS LIMITS AND ANY OTHER DATA MANIPULATION, e.g. rounding, removing NAs
    # if crop yield round the output to 2 decimal places
    
    ##############################################################################
    # Yld
    if (grepl("Yld_Mgha", result)){
      df<-df%>%
        mutate_at(vars(contains('Yld')), ~as.numeric(.))%>% # convert yield columns to numeric, is character
        mutate_at(vars(contains('Yld')), ~round(., 2)) # round to 2 decimal places
      # as_tibble(df) # to check outputs
      
      # check for, report, remove NAs
      n<-sum(is.na(select(df, contains('Yld_Mgha'))))
      if(n>1){
        print(paste(n, ' total NAs in Yld_Mgha this might be normal'))
        # df<-df%>%
        #   select(year, model, climate_scenario, contains('Yld_Mgha'))%>%
          # na.locf()
      }
      
      
      # get min max from county_df
      a<-select(county_df, contains('Yld_Mgha'))
      y_axis_<-c(0, max(a[,1], na.rm = TRUE))
      
    }
    
    ##############################################################################
    # N20
    if (identical(result, 'N2OEmissions_ghayr')){
      
      # check for, report, remove NAs
      n<-sum(is.na(select(df, 'N2OEmissions_ghayr')))
      if(n>1){
        print(paste(n, ' total NAs in N2OEmissions_ghayr'))
        df<-df%>%
          select(year, model, climate_scenario, N2OEmissions_ghayr)
          # na.locf()
      }
      
      # get min max from county_df
      a<-select(county_df, 'N2OEmissions_ghayr')
      y_axis_<-c(0, max(a[,1], na.rm = TRUE))
      # y_axis_<-seq(0, 2600, by = 100)

    }
    
    ##############################################################################
    # CH4
    if (identical(result, 'CH4Emissions_ghayr')){
      
      # check for, report, remove NAs
      n<-sum(is.na(select(df, 'CH4Emissions_ghayr')))
      if(n>1){
        print(paste(n, ' total NAs in CH4Emissions_ghayr'))
        df<-df%>%
          select(year, model, climate_scenario, CH4Emissions_ghayr)
          # na.locf()
      }
      
      a<-select(county_df, 'CH4Emissions_ghayr')
      if(min(a[,1], na.rm = TRUE)>0){
        y_axis_<-c(min(a[,1], na.rm = TRUE), max(a[,1], na.rm = TRUE)) # if result does have positive number make way for plot to work
      } else{
        y_axis_<-c(0, min(a[,1], na.rm = TRUE)) # seems to always be negative? is that true?
      }
     
      
      # y_axis_<-seq(0, -350, by = -50)

    }
    
    ##############################################################################
    # SOC
    if (identical(result, 'SOC_Mghayr')){
      # SOC should really be between 0 and ~15 Kg m-2
      # https://essopenarchive.org/doi/full/10.22541/essoar.168881822.23298383
      # Convert that to Mg ha-1 so multiply by 10 , e.g. 0 and 1000
      # However values are currently between 25 and 75, so sticking with that range at present
      # y_axis_<-seq(0, 100, by = 10)
      
      # check for, report, remove NAs
      n<-sum(is.na(select(df, 'SOC_Mghayr')))
      if(n>1){
        print(paste(n, ' total NAs in SOC_Mghayr'))
        df<-df%>%
          select(year, model, climate_scenario, SOC_Mghayr)
          # na.locf()
      }
      
      a<-select(county_df, 'SOC_Mghayr')
      y_axis_<-c(0, max(a[,1], na.rm = TRUE))
      
    }
    
    ##############################################################################
    # CO2
    if (identical(result, 'CO2resp_ghayr')){
      
      # check for, report, remove NAs
      n<-sum(is.na(select(df, 'CO2resp_ghayr')))
      if(n>1){
        print(paste(n, ' total NAs in CO2resp_ghayr'))
        df<-df%>%
          select(year, model, climate_scenario, CO2resp_ghayr)
          # na.locf()
      }
      
      a<-select(county_df, 'CO2resp_ghayr')
      y_axis_<-c(0, max(a[,1], na.rm = TRUE))
      
    }
    
    # print(y_axis_)
    ##############################################################################
    
    # add model_climate column so ggplot can group by it
    df2<-select(df, year, model, climate_scenario, all_of(result))%>%
      mutate(model_climate = paste0(model, ' ', climate_scenario))
    
    p<-ggplot(df2) +
      geom_line(aes(x=year, y=get(result), group = interaction(model, climate_scenario), 
                    color = model, linetype = climate_scenario)) +
      geom_vline(xintercept=2022, color = 'gray20', linetype="twodash") +
      # scale_y_continuous(breaks = y_axis_) +
      scale_x_continuous(breaks = seq(1850, 2050, by = 20)) +
      ylim(min(y_axis_), max(y_axis_)) +
      xlab('') +
      ylab(result) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    return(p)
  }
  
  scen_1_yield<-gfg_plot(df = s1, result = paste0(cr, 'Yld_Mgha'))
  scen_2_yield<-gfg_plot(df = s2, result = paste0(cr, 'Yld_Mgha'))
  scen_3_yield<-gfg_plot(df = s3, result = paste0(cr, 'Yld_Mgha'))
  scen_4_yield<-gfg_plot(df = s4, result = paste0(cr, 'Yld_Mgha'))
  scen_5_yield<-gfg_plot(df = s5, result = paste0(cr, 'Yld_Mgha'))
  scen_6_yield<-gfg_plot(df = s6, result = paste0(cr, 'Yld_Mgha'))
  
  scen_1_SOC<-gfg_plot(df = s1, result = 'SOC_Mghayr')
  scen_2_SOC<-gfg_plot(df = s2, result = 'SOC_Mghayr')
  scen_3_SOC<-gfg_plot(df = s3, result = 'SOC_Mghayr')
  scen_4_SOC<-gfg_plot(df = s4, result = 'SOC_Mghayr')
  scen_5_SOC<-gfg_plot(df = s5, result = 'SOC_Mghayr')
  scen_6_SOC<-gfg_plot(df = s6, result = 'SOC_Mghayr')
  
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
  
  
  print('saving as grid.arrange object...')

  title<-paste0(cr," -- ", county_name, ", ", state_name, " -- All models high/low radiative forcing climate scenarios")

  left_justify<-.025 # setting as variable to make it easier to adjust
  
  # arrangeGrob does not print to console
  out<-arrangeGrob(arrangeGrob(scen_1_yield, scen_1_N2O, scen_1_CH4, scen_1_CO2, scen_1_SOC, top = grid::textGrob("Monocropping", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_2_yield, scen_2_N2O, scen_2_CH4, scen_2_CO2, scen_2_SOC, top = grid::textGrob("No-till", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_3_yield, scen_3_N2O, scen_3_CH4, scen_3_CO2, scen_3_SOC, top = grid::textGrob("Cover Crop Mix", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_4_yield, scen_4_N2O, scen_4_CH4, scen_4_CO2, scen_4_SOC, top = grid::textGrob("Cover Crop Cereal", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_5_yield, scen_5_N2O, scen_5_CH4, scen_5_CO2, scen_5_SOC, top = grid::textGrob("Cover Crop Legume", x = left_justify, hjust = 0), ncol = 5),
                    arrangeGrob(scen_6_yield, scen_6_N2O, scen_6_CH4, scen_6_CO2, scen_6_SOC, top = grid::textGrob("No-till and Cover Crop Mix", x = left_justify, hjust = 0), ncol = 5),
                    nrow = 6,
                    top = textGrob(title,gp=gpar(fontsize=20,font=3)))
  
  crop_out<-file.path(output_figs, paste0('GEOID_', county_geoid,'_model_results_', cr, ".png"))
  
  ggsave(file = crop_out, plot=out, dpi=300, width = 20, height = 16)
  
  # using png instead of ggsave so that output does not print to console. it will just save the file.
  # png(file = crop_out, width = 20, height = 16) # open png device
  # out # print the plot
  # dev.off() # reset the print device
  
  print(paste0('saved ', cr, ' to ', crop_out))
}
