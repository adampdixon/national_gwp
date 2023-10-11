rm(list=ls())

time <- c("2022-01-01 09:20:00", "2022-01-02 11:10:00", 
          "2022-01-02 18:37:00", "2022-01-03 14:01:00", 
          "2022-01-05 06:50:00", "2022-01-06 17:03:00")
time <- as.POSIXct(time)

value <- c(3, 15, 20, 30, 10, 1)

#time <- soiltemp_df$date
#value <- as.integer(soiltemp_df$soil_temperature)

df <- data.frame(time = time, value = value)

origin <- as.POSIXct("2022-01-01")

df$days <- as.numeric(difftime(time, origin, unit = "day"))

res <- nls(value ~ A * sin(omega * days + phi) + C,  
           data = df, 
           start = list(A = 1, omega = 1, phi = 1, C = 1))

fit <- function(res, newdata) {
  
  x <- as.numeric(difftime(origin, newdata$time, units = "days"))
  C <- as.list(coef(res))
  C$A * sin(C$omega * x + C$phi) + C$C
}

new_times <- seq(0,7,0.1)
#new_times <- seq(0, 1400, 1)
new_df <- data.frame(time = origin + as.difftime(new_times, units = "days"))
new_df$value <- fit(res, new_df)

ggplot(df, aes(time, value)) +
  geom_point() +
  geom_line(data = new_df, colour = "gray") +
  theme_bw()
