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
#      plot(Dates_lag7, residuals_model_2, pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
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
  setNames(paste0(names(.), rep('_lag7', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_SnowDepth, temp_lagged7)

# add lagged (1 days) observations and holidays
temp_lagged1 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(1))) %>% 
  setNames(paste0(names(.), rep('_lag1', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged1)

# add lagged (2 days) observations and holidays
temp_lagged2 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(2))) %>% 
  setNames(paste0(names(.), rep('_lag2', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged2)

# add lagged (3 days) observations and holidays
temp_lagged3 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(3))) %>% 
  setNames(paste0(names(.), rep('_lag3', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged3)

# add lagged (4 days) observations and holidays
temp_lagged4 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(4))) %>% 
  setNames(paste0(names(.), rep('_lag4', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged4)

# add lagged (5 days) observations and holidays
temp_lagged5 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(5))) %>% 
  setNames(paste0(names(.), rep('_lag5', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged5)

# add lagged (6 days) observations and holidays
temp_lagged6 <- tally_by_date_agency_Lagged %>% 
  select(DOT, Holiday) %>% 
  mutate_all(funs(lag, .args = list(6))) %>% 
  setNames(paste0(names(.), rep('_lag6', ncol(.))))
tally_by_date_agency_Lagged <- cbind(tally_by_date_agency_Lagged, temp_lagged6)


# remove first 7 days for plotting
Dates_lag7 <- tail(Dates, -7)

# # model fitting
# model_2_holidays <- lm(formula = DOT ~ Weekday + Holiday + DOT_lag7 + Holiday_lag7 + 
#                          DOT_lag1 + Holiday_lag1 + DOT_lag3 + Holiday_lag3,
#                        data = tally_by_date_agency_Lagged)
# residuals_model_2 <- residuals(model_2_holidays)

##############################################################################################################################################

acf(residuals_model_3, lag.max = 500)

# model fitting
model_4_autocor <- lm(formula = DOT ~ Weekday + Holiday +
                        DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
                        SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
                        SnowDepth.roll100 + SnowDepth.roll200,
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
                DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1,
              data = tally_by_date_agency_Lagged)
residuals_model_5 <- residuals(model_5)


# residuals from model 5
with(tally_by_date_agency_Lagged,
     plot(tail(Dates, -7), residuals_model_5, xlim = range(Dates),
          pch = 20, ylab = "", xaxs = 'i', xaxt = 'n',
          col = ifelse(tail(SnowDepth, -7) > 0, "red", "blue")))
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
legend("topleft", legend = c('SnowDepth > 0', 'SnowDepth = 0'), pch = 20, col = c("red", "blue"), bty = 'n')



######## Generate combined figure ############################################################################################################


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
# with(tally_by_date_agency_Lagged,
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

#### add all other weather variables into the dataframe ######################################################################################

# SnowIce
tally_by_date_agency_all_weather <- tally_by_date_agency_Lagged %>% 
  mutate(SnowIce = weather_NYC_by_var_wide$SnowIce$Average) %>%
  mutate_at("SnowIce", ~replace(., is.na(.), 0))

weather_var_names
plot(weather_NYC_by_var_wide$Rain$Average)

# SnowIce
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>% 
  mutate(DewPoint = weather_NYC_by_var_wide$DewPoint$Average)

# Percipitation
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>% 
  mutate(Percipitation = weather_NYC_by_var_wide$Percipitation$Average) %>%
  mutate_at("Percipitation", ~replace(., is.na(.), 0))

# WindSpeed
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>% 
  mutate(WindSpeed = weather_NYC_by_var_wide$WindSpeed$Average)

# MaxSustainedWind
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>% 
  mutate(MaxSustainedWind = weather_NYC_by_var_wide$MaxSustainedWind$Average)

# Gust
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>% 
  mutate(Gust = weather_NYC_by_var_wide$Gust$Average)

# Rain
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>% 
  mutate(Rain = weather_NYC_by_var_wide$Rain$Average)

# MeanTemp
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>%
  mutate(MeanTemp = weather_NYC_by_var_wide$MeanTemp$Average)

# MinTemp
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>%
  mutate(MinTemp = weather_NYC_by_var_wide$MinTemp$Average)

# MaxTemp
tally_by_date_agency_all_weather <- tally_by_date_agency_all_weather %>%
  mutate(MaxTemp = weather_NYC_by_var_wide$MaxTemp$Average)


save(tally_by_date_agency_all_weather, file = "311_records_with_weather.Rda", version = 2)
