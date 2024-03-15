
# do a x axis with time
# then do an average for all counties, or plot all lines


# Daycent results at county level mapped out
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


# line plots of crops and scenarios
# 
# Dir name
r2<-dir(results_path, recursive=F, full.names=F)
# Dir name and full path
r1<-dir(results_path, recursive=F, full.names=T)
# 





# for all scenarios


# create plots for all crops

for (c in c('Maize', 'Soybeans', 'Wheat', 'Cotton', 'Rotation')) {
  
  for (s in 1:6){
    county_n<-0
    
    maize_scenario1<-data.frame()
    
    for (i in 1:length(r2)){
      files<-list.files(r1[i], full.names=T, recursive=T, pattern='Annual_results_compilation_1_1_Maize_Daycent.csv')
      county_n<-county_n+1
      print(paste0('creating plot ', county_n))
      for (f in files){
        print(paste0('working on ', f))
        county_string<-basename(dirname(f))
        county_string_split<-strsplit(county_string, '_')
        GEOID<-county_string_split[[1]][3]
        State<-county_string_split[[1]][4]
        
        data<-read.csv(f)%>%
          mutate(GEOID=GEOID, State=State)%>%
          select(GEOID, State, year, MaizeYld_Mgha, SOC_Mgha, N2OEmissions_ghayr, CH4Emissions_ghayr)
        
        maize_scenario1<-rbind(maize_scenario1, data)
      }
    }
  
  
  
  # Plot
  
  gfg_plot <- function(){
    p<-ggplot(maize_scenario1, aes(x = year,
                                          y = CH4Emissions_ghayr,
                                          color = GEOID)) +  
    geom_line(show.legend = FALSE) +
    stat_summary(geom="line", fun = "mean", color="black", linewidth=.5) +
    geom_vline(xintercept=2022, color = 'gray20', linetype="dashed") +
    scale_x_continuous(breaks = seq(1850, 2050, by = 10)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
  return(p)
  
  }
  
  scen_1_1<-gfg_plot(type='crop_yld', breaks = crop_breaks, scenario_number = 1)
  scen_1_2<-gfg_plot(type='SOC', breaks = soc_breaks, scenario_number = 1)
  
  scen_2_1<-gfg_plot(type='crop_yld', breaks = crop_breaks, scenario_number = 2)
  scen_2_2<-gfg_plot(type='SOC', breaks = soc_breaks, scenario_number = 2)
  
  scen_3_1<-gfg_plot(type='crop_yld', breaks = crop_breaks, scenario_number = 3)
  scen_3_2<-gfg_plot(type='SOC', breaks = soc_breaks, scenario_number = 3)
  
  scen_4_1<-gfg_plot(type='crop_yld', breaks = crop_breaks, scenario_number = 4)
  scen_4_2<-gfg_plot(type='SOC', breaks = soc_breaks, scenario_number = 4)
  
  scen_5_1<-gfg_plot(type='crop_yld', breaks = crop_breaks, scenario_number = 5)
  scen_5_2<-gfg_plot(type='SOC', breaks = soc_breaks, scenario_number = 5)
  
  scen_6_1<-gfg_plot(type='crop_yld', breaks = crop_breaks, scenario_number = 6)
  scen_6_2<-gfg_plot(type='SOC', breaks = soc_breaks, scenario_number = 6)
  
  
  out<-arrangeGrob(scen_1_1, scen_1_2, 
                   scen_2_1, scen_2_2, 
                   scen_3_1, scen_3_2, 
                   scen_4_1, scen_4_2,
                   scen_5_1, scen_5_2,
                   scen_6_1, scen_6_2,
                   ncol = 2, nrow = 6)
  
  
  crop_out<-file.path(Output, paste0(Crop_, "_Yield_SOC.png"))
  
  ggsave(file = crop_out, plot=out, dpi=300, width = 10, height = 16)
  
  }
  
}






