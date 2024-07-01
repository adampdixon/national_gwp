# Visualize climate data



# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    climate_folder<-'/home/ap/Scratch'
    data_path<-'/home/ap/figs/climate_figs'
    # args=(commandArgs(TRUE))
    # county_number<-1
    
    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    climate_folder<-'/glade/work/apdixon/climate'
    results_path<-'/glade/derecho/scratch/apdixon/national_gwp_figs'
    Test <- FALSE # if TRUE, only run county, filtered below
    # args=(commandArgs(TRUE))
    # county_number = args[2]
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}

library(dplyr)
library(data.table)
library(lubridate)
library(readr)
library(ggplot2)
library(gridExtra)

date<-gsub("-", "", Sys.Date())


# length(unique(low_change_data$GEOID))

# county_summary_low<-low_change_data%>%
#   group_by(GEOID)%>%
#   summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))
# 
# state_summary_low<-low_change_data%>%
#   group_by(State)%>%
#   summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))

# df1<-low_change_data%>%
#   mutate(period = ifelse(year<2022, 'historic', 'future'))%>%
#   group_by(GEOID, period)%>%
#   summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))



clim_hist<-function(grouping, climvar){
  
  low_change_data<-read_csv(file.path(data_path, 'county_climate_low_change_20240329.csv'))
  
  high_change_data<-read_csv(file.path(data_path, 'county_climate_high_change_20240329.csv'))
  
  df_l<-low_change_data%>%
    mutate(period = ifelse(year<2022, 'historic', 'future'))%>%
    group_by({{grouping}}, period)%>%
    summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
    select({{climvar}}, period)

    df_h<-high_change_data%>%
    mutate(period = ifelse(year<2022, 'historic', 'future'))%>%
    group_by({{grouping}}, period)%>%
    summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
    select({{climvar}}, period)
  
  # climvar_<-df1[,climvar]
  
  # Overlaid histograms
  h1<-ggplot(df_l, aes(x={{climvar}}, color = period)) +
    # geom_histogram(fill="white", alpha=0.5, position="identity")
    geom_histogram(fill="white", alpha = .5) +
    ggtitle(paste0("Low Change ", colnames(df_l)[2]))+
    theme_classic()
  
  h2<-ggplot(df_h, aes(x={{climvar}}, color = period)) +
    # geom_histogram(fill="white", alpha=0.5, position="identity")
    geom_histogram(fill="white", alpha = .5) +
    ggtitle(paste0("High Change ", colnames(df_l)[2]))+
    theme_classic()
  
  return(grid.arrange(h1, h2, nrow = 1))
  
}

clim_hist(grouping = State, climvar = mean_tmax)
clim_hist(grouping = State, climvar = mean_tmin)
clim_hist(grouping = State, climvar = mean_precip)
# create histogram



clim_hist2<-function(grouping){
  
  low_change_data<-read_csv(file.path(data_path, 'county_climate_low_change_20240329.csv'))%>%
    filter(year > 2021)%>%
    group_by({{grouping}})%>% # {{grouping}}
    summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
    mutate(clim = 'Low')
  
  high_change_data<-read_csv(file.path(data_path, 'county_climate_high_change_20240329.csv'))%>%
    filter(year > 2021)%>%
    group_by({{grouping}})%>%
    summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
    mutate(clim = 'High')
  
  df<-rbind(low_change_data, high_change_data)

  print(head(df))
  
  # df_h<-high_change_data%>%
  #   # mutate(period = ifelse(year<2022, 'historic', 'future'))%>%
  #   group_by({{grouping}}, period)%>%
  #   summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
  #   select({{climvar}}, period)
  
  # climvar_<-df1[,climvar]
  
  # Overlaid histograms
  h1<-ggplot(df, aes(x=mean_tmin, color = clim)) +
    # geom_histogram(fill="white", alpha=0.5, position="identity")
    geom_histogram(fill="white", alpha = .5) +
    ggtitle("Future mean tmin 2022-2050")+
    theme_classic()
  
  
  h2<-ggplot(df, aes(x=mean_tmax, color = clim)) +
    # geom_histogram(fill="white", alpha=0.5, position="identity")
    geom_histogram(fill="white", alpha = .5) +
    ggtitle("Future mean tmax 2022-2050")+
    theme_classic()
  h3<-ggplot(df, aes(x=mean_precip, color = clim)) +
    # geom_histogram(fill="white", alpha=0.5, position="identity")
    geom_histogram(fill="white", alpha = .5) +
    ggtitle("Future mean precip 2022-2050")+
    theme_classic()
  

  
  return(grid.arrange(h1, h2, h3, nrow = 1))
  
}

clim_hist2(grouping = State)


# create box plots

clim_box<-function(grouping, climvar){
  low_change_data<-fread(file.path(data_path, 'county_climate_low_change_20240329.csv'))%>%
    filter(year > 2021)%>%
    group_by(GEOID, year)%>% # {{grouping}}
    summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
    mutate(clim = 'Low')
  
  high_change_data<-fread(file.path(data_path, 'county_climate_high_change_20240329.csv'))%>%
    filter(year > 2021)%>%
    group_by({{grouping}}, year)%>%
    summarise(mean_tmin=mean(tmin), mean_tmax=mean(tmax), mean_precip=mean(precip))%>%
    mutate(clim = 'High')
  
  df<-rbind(low_change_data, high_change_data)
  
  b1<-ggplot(df, aes(x=as.factor(year), y=mean_tmin, fill=clim)) +
    geom_boxplot() +
    theme_classic() +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  b2<-ggplot(df, aes(x=as.factor(year), y=mean_tmax, fill=clim)) +
    geom_boxplot() +
    theme_classic() +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  b3<-ggplot(df, aes(x=as.factor(year), y=mean_precip, fill=clim)) +
    geom_boxplot() +
    theme_classic() +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(grid.arrange(b1, b2, b3, ncol = 1))
}


clim_box(grouping = State)

clim_box(grouping = State)









