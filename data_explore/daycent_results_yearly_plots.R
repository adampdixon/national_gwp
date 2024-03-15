
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


# line plots of crops and scenarios
# 
# Dir name
r2<-dir(results_path, recursive=F, full.names=F)
# Dir name and full path
r1<-dir(results_path, recursive=F, full.names=T)
# 




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
library("reshape2")   
 
gfg_plot <- ggplot(maize_scenario1, aes(x = year,
                                        y = CH4Emissions_ghayr,
                                        color = GEOID)) +  
  geom_line(show.legend = FALSE) +
  stat_summary(geom="line", fun = "mean", color="black", linewidth=.5) +
  theme_classic()
gfg_plot




