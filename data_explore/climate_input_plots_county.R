#######################################
# Script: climate_input_plots_county.R
# Author: Adam Dixon
# Date: June 2024
# Output: .csv
# Description: This script creates boxplots and histograms of input climate data
#######################################


if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
    Test=TRUE
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
    Test=FALSE
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


low_change_data<-read_csv(file.path(national_figs, paste0('county_climate_low_change_', date, '.csv')))

high_change_data<-read_csv(file.path(national_figs, paste0('county_climate_high_change_', date, '.csv')))


clim_hist<-function(grouping, climvar){
  
  # grouping var is to be flexible how data is aggregated
  # use field name or NULL
  
  df_l<-low_change_data%>%
    mutate(period = ifelse(year<2022, 'historic', 'future'))%>%
    group_by({{grouping}}, period)%>%
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
    select({{climvar}}, period)

    df_h<-high_change_data%>%
    mutate(period = ifelse(year<2022, 'historic', 'future'))%>%
    group_by({{grouping}}, period)%>%
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
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

# clim_hist(grouping = State, climvar = mean_tmax)
# clim_hist(grouping = State, climvar = mean_tmin)
# clim_hist(grouping = State, climvar = mean_precip)
# create histogram



clim_hist2<-function(grouping){
  
  low_change_data<-low_change_data%>%
    filter(year > 2021)%>%
    group_by({{grouping}})%>% # {{grouping}}
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
    mutate(clim = 'Low')
  
  high_change_data<-high_change_data%>%
    filter(year > 2021)%>%
    group_by({{grouping}})%>%
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
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

# clim_hist2(grouping = State)



# create box plots

clim_box<-function(grouping, climvar){
  low_change<-low_change_data%>%
    filter(year > 2021)%>%
    group_by({{grouping}}, year)%>% # {{grouping}}
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
    mutate(clim = 'Low')
  
  high_change<-high_change_data%>%
    filter(year > 2021)%>%
    group_by({{grouping}}, year)%>%
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
    mutate(clim = 'High')
  
  historic_data<-high_change_data%>%
    filter(year <= 2021)%>%
    group_by({{grouping}}, year)%>%
    summarise(mean_tmin=mean(tmin_mean), mean_tmax=mean(tmax_mean), mean_precip=mean(precip_mean))%>%
    mutate(clim = 'Historic')
  
  df<-rbind(low_change, high_change)
  
  a1<-ggplot(historic_data, aes(x=as.factor(year), y=mean_tmin, fill=clim)) +
    geom_boxplot() +
    theme_classic() +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  a2<-ggplot(historic_data, aes(x=as.factor(year), y=mean_tmax, fill=clim)) +
    geom_boxplot() +
    theme_classic() +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  a3<-ggplot(historic_data, aes(x=as.factor(year), y=mean_precip, fill=clim)) +
    geom_boxplot() +
    theme_classic() +
    xlab('') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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
  
  # plots<-grid.arrange(a1, a2, a3, b1, b2, b3, ncol = 1)
  plots<-arrangeGrob(a1, a2, a3, b1, b2, b3, ncol = 1)
  
  ggsave(file = file.path(national_figs, paste0('climate_boxplots', date, '.png')), plot=plots, dpi=300, width = 8, height = 8)
  
  print('climate boxplots saved to')
  print(national_figs)
  # return()
}


clim_box(grouping = State)










