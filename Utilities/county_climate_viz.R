# visualize climate data for each county

mean_clim<-clim_data%>%
  mutate(season = case_when(dayofyear %in% 90:152 ~ "spring",
                            dayofyear %in% c(153:243) ~ "summer",
                            dayofyear %in% c(244:305) ~ "fall",
                            dayofyear %in% c(306:366, 1:89) ~ "winter"))%>%
  group_by(year, season)%>%
  summarise(mean_precip = mean(rain_mm), mean_tavg = mean(tavg), mean_maxt = mean(maxt_C), 
            mean_mint = mean(mint_C), 
            mean_rad = mean(radn_Wm2), meanwind = mean(meanw_ms))%>%
  mutate(x_values = paste0(year, " ", season))

summary(mean_clim$mean_precip)

mean_precip<-ggplot(mean_clim, aes(x=x_values, y=mean_precip)) +
  geom_line(aes(group=1), color="#69b3a2", size=.5) +
  ylim(min(mean_clim$mean_precip), max(mean_clim$mean_precip)) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))
mean_precip

mean_tavg<-ggplot(mean_clim, aes(x=x_values, y=mean_tavg)) +
  geom_line(aes(group=1), color="#69b3a2", size=.5) +
  ylim(min(mean_clim$mean_tavg), max(mean_clim$mean_tavg)) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))
mean_tavg

mean_mint<-ggplot(mean_clim, aes(x=x_values, y=mean_mint)) +
  geom_line(aes(group=1), color="#69b3a2", size=.5) +
  ylim(min(mean_clim$mean_mint), max(mean_clim$mean_mint)) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))
mean_mint


mean_rad<-ggplot(mean_clim, aes(x=x_values, y=mean_rad)) +
  geom_line(aes(group=1), color="#69b3a2", size=.5) +
  ylim(min(mean_clim$mean_rad), max(mean_clim$mean_rad)) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))
mean_rad

meanwind<-ggplot(mean_clim, aes(x=x_values, y=meanwind)) +
  geom_line(aes(group=1), color="#69b3a2", size=.5) +
  ylim(min(mean_clim$meanwind), max(mean_clim$meanwind)) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 3))
meanwind
