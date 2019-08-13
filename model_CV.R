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

#### list of models (formulae) for 1 day ahead ###################################################################

# MSE 177.4523
formula_1_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 176.1181
formula_1_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.3472
formula_1_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 152.8605
formula_1_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 146.0305
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag3 + Holiday_lag3 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1

# MSE 145.6819
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag3 + Holiday_lag3 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain

# MSE 144.7856
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain 

# MSE 141.7736
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + DOT_lag3 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain

# MSE 141.2160
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1

# MSE 139.5853
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain

# MSE 139.5434
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 137.2734
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 137.2508
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag1 + Holiday_lag1 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 136.8604
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 134.7859
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 134.7616
formula_1_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + DOT_lag1 + Holiday_lag1 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

#### list of models (formulae) for 2 day ahead ###################################################################

# MSE 185.3818
formula_2_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 184.8559
formula_2_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 175.309
formula_2_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 167.2041
formula_2_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 167.2041
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag3 + Holiday_lag3 + DOT_lag2 + Holiday_lag2 +
  
  # MSE 164.1116
  formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag3 + Holiday_lag3 + DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain

# MSE 
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain 

# MSE 
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + DOT_lag3 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain

# MSE 156.9906
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + DOT_lag2 + Holiday_lag2

# MSE 154.5388
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain

# MSE 154.5716
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 155.8865
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 155.8822
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 150.0017
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag2 + Holiday_lag2 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 146.3234
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 146.2754
formula_2_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag2 + Holiday_lag2 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

#### list of models (formulae) for 3 day ahead ###################################################################

# MSE 
formula_3_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_3_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_3_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_3_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag4 + Holiday_lag4 + DOT_lag3 + Holiday_lag3 +
  
  # MSE 
  formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag4 + Holiday_lag4 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain

# MSE 
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain 

# MSE 
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain

# MSE 163.8867
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + DOT_lag3 + Holiday_lag3

# MSE 162.2022
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain

# MSE 162.2326
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 155.8865
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 155.8822
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 155.0901
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag3 + Holiday_lag3 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 151.7318
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 155.7571
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag4 + Holiday_lag4 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 151.6781
formula_3_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag3 + Holiday_lag3 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

#### list of models (formulae) for 4 day ahead ###################################################################

# MSE 
formula_4_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_4_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_4_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_4_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag5 + Holiday_lag5 + DOT_lag4 + Holiday_lag4 +
  
  # MSE 
  formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag5 + Holiday_lag5 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain

# MSE 
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain 

# MSE 165.1861
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain

# MSE 166.3373
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + DOT_lag4 + Holiday_lag4

# MSE 162.9546
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain

# MSE 162.991
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 155.8865
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 155.8822
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 156.4804
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag4 + Holiday_lag4 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 152.4238
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 157.8236
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag5 + Holiday_lag5 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 152.3722
formula_4_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag4 + Holiday_lag4 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

#### list of models (formulae) for 5 day ahead ###################################################################

# MSE 
formula_5_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_5_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_5_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_5_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag6 + Holiday_lag6 + DOT_lag5 + Holiday_lag5 +
  
  # MSE 173.0031
  formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag6 + Holiday_lag6 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain

# MSE 
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain 

# MSE 167.7651
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain

# MSE 168.3108
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + DOT_lag5 + Holiday_lag5

# MSE 166.9416
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain

# MSE 167.0113
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 155.8822
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 157.9798
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag5 + Holiday_lag5 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.6033
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 156.9175
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag6 + Holiday_lag6 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.5222
formula_5_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag5 + Holiday_lag5 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

#### list of models (formulae) for 6 day ahead ###################################################################

# MSE 
formula_6_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_6_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_6_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_6_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_6_day_ahead <- DOT ~ Weekday + Holiday  + DOT_lag6 + Holiday_lag6 +
  
  # MSE 
  formula_6_day_ahead <- DOT ~ Weekday + Holiday  + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain

# MSE 173.0276
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain 

# MSE 167.5744
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6

# MSE 167.231
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain

# MSE 167.3224
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 155.8868
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag6 + Holiday_lag6 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.1766
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 155.8868
formula_6_day_ahead <- DOT ~ Weekday + Holiday  + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.0833
formula_6_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + DOT_lag6 + Holiday_lag6 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

#### list of models (formulae) for 7 day ahead ###################################################################

# MSE 
formula_7_day_ahead <- DOT ~ Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_7_day_ahead <- DOT ~ Holiday + 
  DOT_lag7 + Holiday_lag7 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_7_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 
formula_7_day_ahead <- DOT ~ Weekday + 
  DOT_lag7 + Holiday_lag7 +
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 
formula_7_day_ahead <- DOT ~ Weekday + Holiday
  
# MSE 188.4262
formula_7_day_ahead <- DOT ~ Weekday + Holiday + 
  WindSpeed + DewPoint + Rain

# MSE 188.4262
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  WindSpeed + DewPoint + Rain 

# MSE 168.7865
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7

# MSE 168.8085
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  WindSpeed + DewPoint + Rain

# MSE 168.8939
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  WindSpeed + DewPoint + Rain + MeanTemp

# MSE 
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200

# MSE 155.8822
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 + WindSpeed

# MSE 154.4641
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  WindSpeed + DewPoint + Rain + MeanTemp + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.3829
formula_6_day_ahead <- DOT ~ Weekday + Holiday + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 

# MSE 154.3829
formula_7_day_ahead <- DOT ~ Weekday + Holiday +
  DOT_lag7 + Holiday_lag7 + 
  WindSpeed + DewPoint + Rain + 
  SnowDepth.roll7 + SnowDepth.roll30 + SnowDepth.roll60 + 
  SnowDepth.roll100 + SnowDepth.roll200 


#### Evaluation ##########################################################################################

# 480 training / testing split points
train_end_points <- lubridate::ymd("20170101") + seq(1, 480, by = 1) #seq(1, 313, by = 8)

# All data prior to the split point will be used for training 

ground_truth <- predicted_value <- vector(mode = "numeric", length = length(train_end_points))
# loop through the data 
for (i in 1:length(train_end_points)) {
  # set end point for training
  train_end_point <- train_end_points[i]
  # # model fitting
  model_1_day_ahead <- lm(formula = formula_7_day_ahead,
                data = df %>% filter(Date <= train_end_point))
  residuals_model_5 <- residuals(model_1_day_ahead)
  predicted_value[i] <- suppressWarnings(predict(model_1_day_ahead, newdata = df %>% filter(Date == train_end_point + 1)))
  ground_truth[i] <- df %>% filter(Date == train_end_point + 1) %>% pull(DOT)
}

sqrt(mean((predicted_value - ground_truth)^2))

