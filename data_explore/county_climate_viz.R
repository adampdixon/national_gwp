# visualize climate data for each county

# input is daily weather
# column names:
# year dayofyear rain_mm    tavg  maxt_C  mint_C radn_Wm2 meanw_ms


weather_plots<-function(clim_data){
  print("Creating climate plots")
  
  mean_clim<-clim_data%>%
    mutate(season = case_when(dayofyear %in% 90:152 ~ "1 spring",
                              dayofyear %in% c(153:243) ~ "2 summer",
                              dayofyear %in% c(244:305) ~ "3 fall",
                              dayofyear %in% c(306:366, 1:89) ~ "4 winter"))%>%
    group_by(year, season)%>%
    summarise(mean_precip = mean(rain_mm), mean_tavg = mean(tavg), mean_maxt = mean(maxt_C), 
              mean_mint = mean(mint_C), 
              mean_rad = mean(radn_Wm2), meanwind = mean(meanw_ms))%>%
    mutate(year_season = paste0(year, " ", season))%>%
    filter(year > 1945)
  
  # summary(mean_clim$mean_precip)
  
  mean_precip<-ggplot(mean_clim, aes(x=year_season, y=mean_precip)) +
    geom_line(aes(group=1), color="#69b3a2", size=.5) +
    ylim(min(mean_clim$mean_precip), max(mean_clim$mean_precip)) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3),
          axis.title.x=element_blank())

  temp_long<-select(mean_clim, year, year_season, mean_mint, mean_maxt, mean_tavg)%>%
    pivot_longer(cols = c(mean_mint, mean_maxt, mean_tavg), names_to = "var", values_to = "value")
  
  temp_plot<-ggplot(data=temp_long, aes(y=value, x=year_season, color=var, group=var)) +
    geom_line() +
    ylim(min(temp_long$value), max(temp_long$value)) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))

  
  mean_rad<-ggplot(mean_clim, aes(x=year_season, y=mean_rad)) +
    geom_line(aes(group=1), color="#69b3a2", size=.5) +
    ylim(min(mean_clim$mean_rad), max(mean_clim$mean_rad)) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3),
          axis.title.x=element_blank())
  
  
  meanwind<-ggplot(mean_clim, aes(x=year_season, y=meanwind)) +
    geom_line(aes(group=1), color="#69b3a2", size=.5) +
    ylim(min(mean_clim$meanwind), max(mean_clim$meanwind)) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3),
          axis.title.x=element_blank())
  
  
  # save plots
  
  # mean_precip
  # meanwind
  # mean_rad
  # temp_plot
  
  out<-arrangeGrob(mean_precip, 
                   meanwind,
                   mean_rad,
                   temp_plot,
                   ncol = 1, nrow = 4)
  
  # create figs dir if doesn't exist
  
  out_path<-figs_input_data

  plot_out<-file.path(out_path, paste0(site_name, "_input_climate_figs.png"))
  
  ggsave(file = plot_out, plot=out, dpi=300, width = 8.5, height = 10)
  
  print("Climate plots saved")
  
  
}
  
weather_plots(clim_data = clim_data)
  
  # mean_temp<-ggplot(mean_clim, aes(x=year_season)) +
  #   geom_line(aes(mint)) +
  #   # ylim(min(mean_clim$mean_mint), max(mean_clim$mean_mint)) +
  #   theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))
  # mean_temp
  # mean_maxt<-ggplot(mean_clim, aes(x=year_season, y=mean_maxt)) +
  #   geom_line(aes(group=1), color="#69b3a2", size=.5) +
  #   ylim(min(mean_clim$mean_mint), max(mean_clim$mean_mint)) +
  #   theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))

