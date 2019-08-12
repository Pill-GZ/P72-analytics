rm(list = ls())
library(dplyr)
library(purrr)
# setwd("/data/gaozheng/P72-analytics/")

#### Load weather and 311 records #######################################################################

# install.packages('bit64')
library(data.table)
tally_by_date_agency_wide <- data.table::fread("311_records_by_date_agency.csv")
tally_by_date_agency_wide$Date <- as.Date(tally_by_date_agency_wide$Date)

load("weather_NYC.Rdata")
# head(weather_NYC_by_var_wide$MeanTemp)


#### Reference variables ################################################################################

agencyTypes_collapsed <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 
                           'DOB', 'DPR', 'DOHMH', 'DOF', 'TLC',
                           'Others')

weather_var_names <- c('MeanTemp', 'MinTemp', 'MaxTemp', 'DewPoint', 
                       'Percipitation', 'WindSpeed', 'MaxSustainedWind', 
                       'Gust', 'Rain', 'SnowDepth', 'SnowIce')

stations_NYC <- c("Station.26", "Station.27", "Station.28", "Station.4", "Station.50", "Station.9" )


#### Align 311 records with weather information ########################################################

# range of dates with weather information
Dates <- weather_NYC_by_var_wide$MeanTemp$Date
range(Dates)

# filter 311 records down to where weather data is available
tally_by_date_agency_wide <- tally_by_date_agency_wide %>% 
  filter(Date <= max(Dates))


#### create features -- day-of-the-week #################################################################

tally_by_date_agency_Weekdays <- tally_by_date_agency_wide %>% 
  mutate(Weekday = lubridate::wday(Date, label=TRUE))

par(mar = c(2,5,1,1), mfrow = c(5,1), las = 1)
with(tally_by_date_agency_Weekdays,
     plot(Date, DOT, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(Weekday %in% c('Sun', 'Sat'), "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('Weekends', 'Weekdays'), pch = 20, col = c("red", "blue"), bty = 'n')

# model fitting
model_1_weekdays <- lm(formula = DOT ~ Weekday,
                       data = tally_by_date_agency_Weekdays)
residuals_model_1 <- residuals(model_1_weekdays)

#### create features -- holidays ####################################################

holidays <- read.csv(file = "Holidays_2010-2018.tsv", sep = "\t", stringsAsFactors = F,
                     header = F, col.names = c("Holiday", "Date"))
holidays <- holidays %>% 
  mutate(Date = lubridate::mdy(Date),
         Holiday = as.factor(holidays$Holiday)) %>% 
  filter(Date < max(Dates))

# residuals from model 1
with(tally_by_date_agency_Weekdays,
     plot(Date, residuals_model_1, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(Date %in% holidays$Date, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('Holidays', 'Non-holidays'), pch = 20, col = c("red", "blue"), bty = 'n')

# add holidays to the mix
tally_by_date_agency_Holidays <- tally_by_date_agency_Weekdays %>% 
  left_join(holidays, by = "Date") %>% 
  mutate(Holiday = addNA(Holiday))

# model fitting
model_2_holidays <- lm(formula = DOT ~ Weekday + Holiday,
                       data = tally_by_date_agency_Holidays)
residuals_model_2 <- residuals(model_2_holidays)


#### create features -- temperature #################################################################

library(zoo)
k <- 7
weather_NYC_by_var_wide$MeanTemp$Weekly.average <- 
  c(rep(NA, k-1), rollmean(weather_NYC_by_var_wide$MeanTemp$Average, k = k))
weather_NYC_by_var_wide$MeanTemp$Weekly.decrease <- 
  c(rep(NA, k), tail(weather_NYC_by_var_wide$MeanTemp$Weekly.average, -k) - 
      head(weather_NYC_by_var_wide$MeanTemp$Weekly.average, -k))
k <- 30
weather_NYC_by_var_wide$MeanTemp$Monthly.average <- 
  c(rep(NA, k-1), rollmean(weather_NYC_by_var_wide$MeanTemp$Average, k = k))
weather_NYC_by_var_wide$MeanTemp$Monthly.decrease <- 
  c(rep(NA, k), tail(weather_NYC_by_var_wide$MeanTemp$Monthly.average, -k) - 
      head(weather_NYC_by_var_wide$MeanTemp$Monthly.average, -k))

# plot(Dates, weather_NYC_by_var_wide$MeanTemp$Weekly.average, type = 'b')
# plot(Dates, weather_NYC_by_var_wide$MeanTemp$Weekly.decrease, type = 'b')
# plot(Dates, weather_NYC_by_var_wide$MeanTemp$Monthly.average, type = 'b')
# plot(Dates, weather_NYC_by_var_wide$MeanTemp$Monthly.decrease, type = 'b')

tally_by_date_agency_Temp <- tally_by_date_agency_Holidays %>% 
  mutate(Temperature = weather_NYC_by_var_wide$MeanTemp$Average)
tally_by_date_agency_Temp <- tally_by_date_agency_Temp %>% 
  mutate(Monthly.decrease = weather_NYC_by_var_wide$MeanTemp$Monthly.decrease)
tally_by_date_agency_Temp <- tally_by_date_agency_Temp %>% 
  mutate(Monthly.decrease = weather_NYC_by_var_wide$MeanTemp$Monthly.decrease)

# with(tally_by_date_agency_Temp,
#      plot(Dates.lag7, residuals_model_2, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
#           col = ifelse(Temperature < 40, "red", "blue")))
# axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
# legend("topleft", legend = c('Temp < 40', 'Temp > 40'), pch = 20, col = c("red", "blue"), bty = 'n')

#### create features -- snowDepth #################################################################

# SnowDepth
tally_by_date_agency_SnowDepth <- tally_by_date_agency_Temp %>% 
  mutate(SnowDepth = weather_NYC_by_var_wide$SnowDepth$Average) %>%
  mutate_at("SnowDepth", ~replace(., is.na(.), 0))

# residuals from model 2
with(tally_by_date_agency_SnowDepth,
     plot(Dates, residuals_model_2, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(SnowDepth > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, col = c("red", "blue"), bty = 'n')


# SnowDepth rolling averages
k <- 7
tally_by_date_agency_SnowDepth <- tally_by_date_agency_SnowDepth %>%
  mutate(SnowDepth.roll7 = rollmean(SnowDepth, k = k, fill = c(NA,0,0), align = "right"))
k <- 30
tally_by_date_agency_SnowDepth <- tally_by_date_agency_SnowDepth %>%
  mutate(SnowDepth.roll30 = rollmean(SnowDepth, k = k, fill = c(NA,0,0), align = "right"))
k <- 60
tally_by_date_agency_SnowDepth <- tally_by_date_agency_SnowDepth %>%
  mutate(SnowDepth.roll60 = rollmean(SnowDepth, k = k, fill = c(NA,0,0), align = "right"))
k <- 100
tally_by_date_agency_SnowDepth <- tally_by_date_agency_SnowDepth %>%
  mutate(SnowDepth.roll100 = rollmean(SnowDepth, k = k, fill = c(NA,0,0), align = "right"))
k <- 200
tally_by_date_agency_SnowDepth <- tally_by_date_agency_SnowDepth %>%
  mutate(SnowDepth.roll200 = rollmean(SnowDepth, k = k, fill = c(NA,0,0), align = "right"))

# model fitting
model_3_snow <- lm(formula = DOT ~ Weekday + Holiday + SnowDepth.roll7 +
                     SnowDepth.roll30 + SnowDepth.roll60 + 
                     SnowDepth.roll100 + SnowDepth.roll200,
                   data = tally_by_date_agency_SnowDepth)
residuals_model_3 <- residuals(model_3_snow)


# residuals from model 3
with(tally_by_date_agency_SnowDepth,
     plot(tail(Dates, -199), residuals_model_3, xlim = range(Dates),
          pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(tail(SnowDepth, -199) > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, col = c("red", "blue"), bty = 'n')


#### create features -- lagged observations #################################################################

# add lagged (7 days) observations and holidays
temp_lagged7 <- tally_by_date_agency_SnowDepth %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(7))) %>% 
  setNames(paste0(names(.), rep('.lag7', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_SnowDepth, temp_lagged7)

# add lagged (1 days) observations and holidays
temp_lagged1 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(1))) %>% 
  setNames(paste0(names(.), rep('.lag1', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged1)

# add lagged (3 days) observations and holidays
temp_lagged3 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(3))) %>% 
  setNames(paste0(names(.), rep('.lag3', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged3)

# remove first 7 days for plotting
Dates.lag7 <- tail(Dates, -7)

# # model fitting
# model_2_holidays <- lm(formula = DOT ~ Weekday + Holiday + DOT.lag7 + Holiday.lag7 + 
#                          DOT.lag1 + Holiday.lag1 + DOT.lag3 + Holiday.lag3,
#                        data = tally_by_date_agency_Lagged)
# residuals_model_2 <- residuals(model_2_holidays)

##############################################################################################################################################

acf(residuals_model_4, lag.max = 500)

# model fitting
model_4_autocor <- lm(formula = DOT ~ Weekday + Holiday +
                        DOT.lag7 + Holiday.lag7 + DOT.lag1 + Holiday.lag1 + 
                        SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
                        SnowDepth.roll100 + SnowDepth.roll200 +
                        DOT.lag7 + Holiday.lag7 + DOT.lag1 + Holiday.lag1,
                      data = tally_by_date_agency_Lagged)
residuals_model_4 <- residuals(model_4_autocor)


# residuals from model 4
with(tally_by_date_agency_SnowDepth,
     plot(tail(Dates, -199), residuals_model_4, xlim = range(Dates),
          pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(tail(SnowDepth, -199) > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, col = c("red", "blue"), bty = 'n')



# model fitting
model_5 <- lm(formula = DOT ~ Weekday + Holiday + 
                DOT.lag7 + Holiday.lag7 + DOT.lag1 + Holiday.lag1,
              data = tally_by_date_agency_Lagged)
residuals_model_5 <- residuals(model_5)


# residuals from model 5
with(tally_by_date_agency_Lagged,
     plot(tail(Dates, -7), residuals_model_5, xlim = range(Dates),
          pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(tail(SnowDepth, -7) > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, col = c("red", "blue"), bty = 'n')



##############################################################################################################################################


par(mar = c(2,4,1,1), mfrow = c(4,1), las = 1, cex = 1)
with(tally_by_date_agency_Weekdays,
     plot(Date, DOT, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n', cex = 0.5,
          col = ifelse(Weekday %in% c('Sun', 'Sat'), "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('Weekends', 'Weekdays'), pch = 20, 
       col = c("red", "blue"), bg = grey(0.9), box.lwd = 0)
text(x = lubridate::ymd("20180501"), y = 2500, adj = 1, labels = "DOT incoming calls")

# residuals from model 1
with(tally_by_date_agency_Weekdays,
     plot(Date, residuals_model_1, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n', cex = 0.5,
          col = ifelse(Date %in% holidays$Date, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('Holidays', 'Non-holidays'), pch = 20, 
       col = c("red", "blue"),  bg = grey(0.9), box.lwd = 0)
text(x = lubridate::ymd("20180501"), y = 1400, adj = 1, labels = "Residuals from Model 1")

# residuals from model 2
with(tally_by_date_agency_SnowDepth,
     plot(Dates, residuals_model_2, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n', cex = 0.5,
          col = ifelse(SnowDepth > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, 
       col = c("red", "blue"), bg = grey(0.9), box.lwd = 0)
text(x = lubridate::ymd("20180501"), y = 1400, adj = 1, labels = "Residuals from Model 2")

# # residuals from model 3
# with(tally_by_date_agency_SnowDepth,
#      plot(tail(Dates, -199), residuals_model_3, xlim = range(Dates),
#           pch = 20, ylab = "", xaxs = 'i', xaxt = 'n', cex = 0.5,
#           col = ifelse(tail(SnowDepth, -199) > 0, "blue", "blue")))
# axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
# legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, 
#        col = c("red", "blue"),  bg = grey(0.9), box.lwd = 0)

# residuals from model 4
with(tally_by_date_agency_Lagged,
     plot(tail(Dates, -199), residuals_model_4, xlim = range(Dates),
          pch = 20, ylab = "", xaxs = 'i', xaxt = 'n', cex = 0.5,
          col = ifelse(tail(SnowDepth, -199) > 0, "blue", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
# legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, 
#        col = c("red", "blue"),  bg = grey(0.9), box.lwd = 0)
text(x = lubridate::ymd("20180501"), y = 1400, adj = 1, labels = "Residuals from Model 3")

# # residuals from model 5
# with(tally_by_date_agency_SnowDepth,
#      plot(tail(Dates, -7), residuals_model_5, xlim = range(Dates),
#           pch = 20, ylab = "", xaxs = 'i', xaxt = 'n', cex = 0.5,
#           col = ifelse(tail(SnowDepth, -7) > 0, "red", "blue")))
# axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
# legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, 
#        col = c("red", "blue"),  bg = grey(0.9), box.lwd = 0)

sd(residuals_model_1)
sd(residuals_model_2)
sd(residuals_model_3)
sd(residuals_model_4)
sd(residuals_model_5)

##############################################################################################################################################

# SnowIce
tally_by_date_agency_SnowIce <- tally_by_date_agency_SnowDepth %>% 
  mutate(SnowIce = weather_NYC_by_var_wide$SnowIce$Average) %>%
  mutate_at("SnowIce", ~replace(., is.na(.), 0))

# plot residuals marking snow periods
with(tally_by_date_agency_SnowIce,
     plot(Dates, residuals_model_2, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(SnowDepth > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, col = c("red", "blue"), bty = 'n')


with(tally_by_date_agency_SnowIce,
     plot(Dates, residuals_model_2, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(SnowIce > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowIce > 0', 'SnowIce = 0'), pch = 20, col = c("red", "blue"), bty = 'n')


acf(residuals_model_2, lag.max = 500)

par(mfrow = c(5,1))
plot(tail(tally_by_date_agency_Temp$Temperature, -7), residuals_model_2)
plot(tail(weather_NYC_by_var_wide$MeanTemp$Average, -8), tail(residuals_model_2, -1))
plot(tail(weather_NYC_by_var_wide$MeanTemp$Weekly.average, -7), residuals_model_2)
plot(tail(weather_NYC_by_var_wide$MeanTemp$Weekly.decrease, -7), residuals_model_2)
plot(tail(weather_NYC_by_var_wide$MeanTemp$Monthly.decrease, -7), residuals_model_2)

plot(weather_NYC_by_var_wide$MeanTemp$Average, residuals_model_3)
plot(head(weather_NYC_by_var_wide$MeanTemp$Average, -1), tail(residuals_model_3, -1))
plot(weather_NYC_by_var_wide$MeanTemp$Weekly.average, residuals_model_3)
plot(weather_NYC_by_var_wide$MeanTemp$Weekly.decrease, residuals_model_3)
plot(weather_NYC_by_var_wide$MeanTemp$Monthly.decrease, residuals_model_3)


# model fitting
model_3_temperature <- lm(formula = DOT ~ Weekday + Holiday + DOT.lag7 + Holiday.lag7 + 
                            DOT.lag1 + Holiday.lag1 + DOT.lag3 + Holiday.lag3 + 
                            poly(Temperature, 2), # + I(Monthly.decrease < -3),
                          data = tally_by_date_agency_Temp)
residuals_model_3 <- residuals(model_3_temperature)


with(tally_by_date_agency_Temp,
     plot(Date[!is.na(Monthly.decrease)], residuals_model_3, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(Temperature < 40, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('Temp < 40', 'Temp > 40'), pch = 20, col = c("red", "blue"), bty = 'n')



Date_7 <- tail(Dates, -7)

with(tally_by_date_agency_Temp,
     plot(Date_7, -diff(DOT, lag = -7), pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(Date %in% holidays$Date, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('Temp < 40', 'Temp > 40'), pch = 20, col = c("red", "blue"), bty = 'n')


