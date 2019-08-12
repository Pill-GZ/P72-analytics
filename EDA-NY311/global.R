
library(plotly)
library(shiny)
library(dplyr)
library(purrr)

df <- data.table::fread("data/311_records_Jan2015.csv")
# df <- df %>% 
#   mutate(Date = lubridate::mdy(substr(Created.Date, start = 1, stop = 10)))

tally_by_date_agency_wide <- data.table::fread("data/311_records_by_date_agency.csv")
tally_by_date_agency_wide$Date <- as.Date(tally_by_date_agency_wide$Date)

load("data/weather_NYC.Rdata")

eventColors <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', 
                 '#ffff33', '#a65628', '#f781bf', '#999999')

# 1 HPD 2 NYPD 3 DOT 4 DSNY 5 DEP 6 DOB 7 DPR 8 DOHMH 9 DOF 10 TLC
agencyTypes <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 'DOB', 'DPR', 'DOHMH')

agencyTypes_collapsed <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 
                           'DOB', 'DPR', 'DOHMH', 'DOF', 'TLC',
                           'Others')

weather_var_names <- c('MeanTemp', 'MinTemp', 'MaxTemp', 'DewPoint', 
                       'Percipitation', 'WindSpeed', 'MaxSustainedWind', 
                       'Gust', 'Rain', 'SnowDepth', 'SnowIce')
# x-axis dates
Dates <- weather_NYC_by_var_wide$MeanTemp$Date
