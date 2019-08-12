rm(list = ls())
library(dplyr)
library(purrr)
# setwd("/data/gaozheng/P72-analytics/")

#### Load 311 records with weather info  #######################################################################

# install.packages('bit64')
load("311_records_with_weather.Rda")
# head(weather_NYC_by_var_wide$MeanTemp)
df <- tally_by_date_agency_all_weather

#### Reference variables ################################################################################

agencyTypes_collapsed <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 
                           'DOB', 'DPR', 'DOHMH', 'DOF', 'TLC',
                           'Others')

weather_var_names <- c('MeanTemp', 'MinTemp', 'MaxTemp', 'DewPoint', 
                       'Percipitation', 'WindSpeed', 'MaxSustainedWind', 
                       'Gust', 'Rain', 'SnowDepth', 'SnowIce')

stations_NYC <- c("Station.26", "Station.27", "Station.28", "Station.4", "Station.50", "Station.9" )

#### Set training and testing schemes ###################################################################

# list of models (formulae)
formula_0 <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

formula_0 <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

formula_1 <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

formula_2 <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1

formula_3 <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

formula_4 <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

formula_4 <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain

# 60 training / testing split points
train_end_points <- lubridate::ymd("20170101") + seq(1, 480, by = 4) #seq(1, 313, by = 8)

# All data prior to the split point will be used for training 

ground_truth <- predicted_value <- vector(mode = "numeric", length = length(train_end_points))
# loop through the data 
for (i in 1:length(train_end_points)) {
  # set end point for training
  train_end_point <- train_end_points[i]
  # # model fitting
  model_1_day_ahead <- lm(formula = formula_4,
                data = df %>% filter(Date <= train_end_point))
  residuals_model_5 <- residuals(model_1_day_ahead)
  predicted_value[i] <- suppressWarnings(predict(model_1_day_ahead, newdata = df %>% filter(Date == train_end_point + 1)))
  ground_truth[i] <- df %>% filter(Date == train_end_point + 1) %>% pull(DOT)
}

sqrt(mean((predicted_value - ground_truth)^2))

# summary(predicted_value - ground_truth)
# par(mfrow = c(1,1))
# plot(ground_truth, predicted_value)
abline(a = 0, b = 1, lty =2)


