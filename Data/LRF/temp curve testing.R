rm(list=ls())

soiltemp_df <- read.csv("Data/LRF/soiltemp.csv")
soiltemp_df$date <- as.POSIXct(soiltemp_df$date)

origin <- as.POSIXct("2004-01-01 00:00:00")

soiltemp_df <- soiltemp_df[soiltemp_df$date >= "2004-02-01" & soiltemp_df$date < "2007-02-01",]

soiltemp_df$days <-as.numeric(difftime(soiltemp_df$date, origin, unit = "day"))

A<- (max(soiltemp_df$soil_temperature,na.rm=T)-min(soiltemp_df$soil_temperature,na.rm=T)/2)
C<-((max(soiltemp_df$soil_temperature,na.rm=T)+min(soiltemp_df$soil_temperature,na.rm=T))/2)

# res <- nls(soil_temperature ~ A * sin(omega * days + phi) + C,  
#            data = soiltemp_df, 
#            start = list(A = A, omega = .006, phi = 10, C = C))
#A=12.7, omega=0.0171, phi=4.53, C=17.6

res <- nls(soil_temperature ~ A * sin(omega * days + phi) + C,  
           data = soiltemp_df, 
           start = list(A = A, omega = .006, phi = 10, C = C))


fit <- function(res, newdata) {

  x <- as.numeric(difftime(origin, newdata$date, units = "days"))
  C <- as.list(coef(res))
  #C$A * sin(C$omega * x + C$phi) + C$C 
  12.7 * sin(0.0169 * x + 4.53) + 17.6 
}

soiltemp_df$value <- fit(res, soiltemp_df)

ggplot(soiltemp_df, aes(date, soil_temperature)) +
  geom_point() +
  geom_line(aes(x=date,y=value, colour = "red"))




xc<-cos(2*pi*days/366)
xs<-sin(2*pi*days/366)
fit.lm <- lm(soiltemp_df$soil_temperature~xc+xs)
fit <- fitted(fit.lm)
# find predicted values for original time series
pred <- predict(fit.lm, newdata=data.frame(days=days))    
pred_df1 <- data.frame(pred=pred,days=days)

# find linear trend
pred_fit <- lm(pred ~ days)
pred_fit_coef <- coef(pred_fit)
pred_fit_slope_byday <- pred_fit_coef[2]
pred_slope_byyear <- pred_fit_slope_byday*365

pred_df1 %>%
  ggplot(aes(x=days, y=pred, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=pred_fit_coef[1], slope=pred_fit_coef[2], color="red") +
  ggtitle(" Observed Soil Temperature") +
  xlab("Days") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())




pred_df <- data.frame(pred=pred,date=soiltemp_df$date,year=soiltemp_df$year)

pred_ObsTfit <- lm(pred ~ days)
pred_ObsTfit_coef <- coef(pred_ObsTfit)
pred_ObsTfit_r2 <- round(summary(pred_ObsTfit)$r.squared,2)
pred_ObsTfit_slope_byday <- pred_ObsTfit_coef[2]

gT2 <- soiltemp_df %>%
  ggplot(aes(x=as.numeric(difftime(date,origin,unit="day")), y=soil_temperature, show.legend=TRUE)) +
  geom_line() +
  geom_line(aes(days,pred,color="blue")) +
  geom_abline(intercept=pred_ObsTfit_coef[1], slope=pred_ObsTfit_coef[2], color="red") +
  ggtitle(paste0(site_name," Observed Soil Temperature")) +
  xlab("Year") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT2

pred_ObsTfita <- lm(pred ~ date,data=pred_df)
pred_ObsTfit_coefa <- coef(pred_ObsTfita)
pred_ObsTfit_r2a <- round(summary(pred_ObsTfita)$r.squared,2)
pred_ObsTfit_slope_bydaya <- pred_ObsTfit_coefa[2]

gT2a <- soiltemp_df %>%
  ggplot(aes(x=date, y=soil_temperature, show.legend=TRUE)) +
  geom_line() +
  geom_line(aes(date,pred,color="blue")) +
  geom_abline(intercept=pred_ObsTfit_coefa[1], slope=pred_ObsTfit_coefa[2], color="red") +
  ggtitle(paste0(site_name," Observed Soil Temperature")) +
  xlab("Year") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT2a

## find annual trend of predicted values to compare to observations
pred_ObsTfit2 <- lm(pred ~ year, data = pred_df)
pred_ObsTfit_coef2 <- coef(pred_ObsTfit2)
pred_ObsTfit_slope_byyear <- pred_ObsTfit_coef2[2]

gT2b <- pred_df %>%
  ggplot(aes(x=date, y=pred, show.legend=TRUE)) +
  geom_line() +
  geom_line(aes(date,pred,color="blue")) +
  geom_abline(intercept=pred_ObsTfit_coef[1], slope=pred_ObsTfit_coef[2], color="red") +
  ggtitle(paste0(site_name," Observed Soil Temperature")) +
  xlab("Year") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT2b
