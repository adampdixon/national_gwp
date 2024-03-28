
# do a x axis with time
# then do an average for all counties, or plot all lines

# version 2 is to plot both climate scenarios on same plot


# Daycent results at yearly level in ggplot
# January 15 2023
# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_path<-'/home/ap/Daycent_results'
    crop_area_path<-file.path(master_path, 'Data', 'County_start', 'main_county_crops.csv')
    output_figs<-'/home/ap/figs'
    county_number<-1

    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_path<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    crop_area_path<-file.path(master_path, 'Data', 'County_start', 'main_county_crops.csv')
    output_figs<-'/glade/derecho/scratch/apdixon/national_gwp_figs'
    
    # county_number<-args[2]
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}


library(dplyr)
library(ggplot2)
library(reshape2) 
library(gridExtra)
library(grid)
library(data.table)
# library(patchwork)


# line plots of crops and scenarios
# 
# Dir name
r2<-dir(results_path, recursive=F, full.names=F)
# Dir name and full path
r1<-dir(results_path, recursive=F, full.names=T)
# 





# for all scenarios


# create plots for all crops


for (cr in c('Maize', 'Soybean', 'Wheat', 'Cotton', 'Rotation')) {
  crop_scenario_df<-data.frame()
  for (s in 1:6){
    print(paste0('working on ', cr, ', practice scenario ', s, ' table, ', 'in both climate scenarios '))
    
    # county_n<-0 # counter
    
    for (i in 1:length(r2)){
      files_1<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('Annual_results_compilation_1_', s, '_', cr, '_Daycent.csv'))
      files_2<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('Annual_results_compilation_2_', s, '_', cr, '_Daycent.csv'))
      # county_n<-county_n+1
      
      files<-c(files_1, files_2)
      
      for (f in files){
        # print(paste0('working on ', f))
        county_string<-basename(dirname(f))
        county_string_split<-strsplit(county_string, '_')
        GEOID<-county_string_split[[1]][3]
        State<-county_string_split[[1]][4]
        
        if(identical('Rotation', cr)){ # deal with Rotation having maize and soybean yield by pulling both columns in select below
          select_var = c('Maize', 'Soybean')
        } else {
          select_var = cr
        }
        
        data<-fread(f)%>%
          mutate(GEOID=GEOID, State=State,
                 climate_scen = ifelse(climate_scenario_num == 1, 'low', 'high'))%>%
          select(GEOID, State, year, mgmt_scenario_num, climate_scen,
                 paste0(select_var, 'Yld_Mgha'), SOC_Mgha, N2OEmissions_ghayr, CH4Emissions_ghayr, CO2resp_ghayr)
        
        if(identical('Rotation', cr)){ # populate new column called 'RotationYld_Mgha' depending on which one is NA
          data$RotationYld_Mgha <- ifelse(is.na(data$MaizeYld_Mgha), data$SoybeanYld_Mgha, data$MaizeYld_Mgha)
        }
        
        data$Scenario<-s
        crop_scenario_df<-rbind(crop_scenario_df, data)
        
        
        
      } # end of for loop listing files
      
    } # end of for loop reading files
    
  } # end of for loop scenarios
  
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
  
  print('plotting')
  
  # MaizeYld_Mgha, SOC_Mgha, N2OEmissions_ghayr, CH4Emissions_ghayr
  
  s1<-filter(crop_scenario_df, Scenario == 1)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  
  
  
  
  s2<-filter(crop_scenario_df, Scenario == 2)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s3<-filter(crop_scenario_df, Scenario == 3)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s4<-filter(crop_scenario_df, Scenario == 4)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s5<-filter(crop_scenario_df, Scenario == 5)%>%
    group_by(year, climate_scen) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  s6<-filter(crop_scenario_df, Scenario == 6)%>%
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
  
  
  
  crop_out<-file.path(output_figs, paste0('both_clim_scen_', cr,  "_results.png"))
  
  ggsave(file = crop_out, plot=out, dpi=300, width = 20, height = 16)
  
  print('done')
  
} # end of crop loop









