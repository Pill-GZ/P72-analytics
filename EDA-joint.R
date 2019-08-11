rm(list = ls())
library(dplyr)
library(purrr)

#### Load weather and 311 records #######################################################################

# install.packages('bit64')
library(data.table)
tally_by_date_agency_wide <- data.table::fread("311_records_by_date_agency.csv")
tally_by_date_agency_wide$Date <- as.Date(tally_by_date_agency_wide$Date)

load("weather_NYC.Rdata")

#### Reference variables ################################################################################

agencyTypes_collapsed <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 
                           'DOB', 'DPR', 'DOHMH', 'DOF', 'TLC',
                           'Others')

weather_var_names <- c('MeanTemp', 'MinTemp', 'MaxTemp', 'DewPoint', 
                       'Percipitation', 'WindSpeed', 'MaxSustainedWind', 
                       'Gust', 'Rain', 'SnowDepth', 'SnowIce')

stations_NYC <- c("Station.26", "Station.27", "Station.28", "Station.4", "Station.50", "Station.9" )

#### traceplots -- 311 call records #####################################################################

# traceplot - total number of calls (useless for pattern discovery)
par(mar = c(2,5,0,1), las = 1, mfrow = c(1,1))
plot(x = tally_by_date_agency_wide$Date, y = rowSums(tally_by_date_agency_wide[,-1]), 
     xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
     type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
legend("topleft", legend = agency, bty = 'n')
axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))

# matplot - all top 6 categories together
par(mar = c(2,5,1,1), las = 1, mfrow = c(1,1))
matplot(x = tally_by_date_agency_wide$Date, y = tally_by_date_agency_wide[,2:7], 
        xlim = range(tally_by_date_agency_wide$Date) - c(0,50), ylim = c(20, 8000),
        type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
legend("topleft", legend = agencyTypes_collapsed[1:5], col = 1:5, lty = 1)

# separate traceplots - first 5 agencies
par(mar = c(2,5,0,1), las = 1, mfrow = c(5,1))
for (agency in agencyTypes_collapsed[1:5]) {
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
       xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  legend("topleft", legend = agency, bty = 'n')
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}

# separate traceplots - next 5 agencies
par(mar = c(2,5,0,1), las = 1, mfrow = c(5,1))
for (agency in agencyTypes_collapsed[6:10]) {
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
       xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  legend("topleft", legend = agency, bty = 'n')
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}

# separate traceplots - the rest combined
par(mar = c(2,5,0,1), las = 1, mfrow = c(1,1))
for (agency in agencyTypes_collapsed[11]) {
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
       xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  legend("topleft", legend = agency, bty = 'n')
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}

#### traceplots -- weather variables averaged ###########################################################

par(mar = c(3,3,1,1), mfrow = c(5,1))
for (weather_var_name in weather_var_names[1:5]) {
  plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'b')
  legend("topleft", legend = weather_var_name)
}

plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'b', log = 'y',  ylim = c(0.0025, 2))

par(mar = c(3,3,1,1), mfrow = c(6,1))
for (weather_var_name in weather_var_names[6:11]) {
  plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'b')
  legend("topleft", legend = weather_var_name)
}

# temp fluctuation within a day
plot(weather_NYC_by_var_wide %>% pluck('MaxTemp', 'Average') - 
       weather_NYC_by_var_wide %>% pluck('MinTemp', 'Average'), type = 'b')


#### traceplots -- 311 call records shaded by weather variables averaged ###########################################################

# x-axis dates
Dates <- weather_NYC_by_var_wide$MeanTemp$Date

# set quantiles of weather variables
weather_var_name <- weather_var_names[10]
# "MeanTemp"         "MinTemp"          "MaxTemp"          "DewPoint"        
# "Percipitation"    "WindSpeed"        "MaxSustainedWind" "Gust"            
# "Rain"             "SnowDepth"        "SnowIce"         
weather_var_val <- weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average')
summary(weather_var_val)
shade_val_above <- T
percentile <- 0.01
(threshold <- quantile(weather_var_val, probs = percentile, na.rm = T))


# shaded traceplots - first 5 agencies
par(mar = c(2,5,0,1), las = 1, mfrow = c(5,1))
for (agency in agencyTypes_collapsed[1:5]) {
  # create empty plot
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
       xlim = range(Dates) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  
  # shade weather variables
  threshold_exceedance <- if (shade_val_above) { 
    weather_var_val > threshold
  } else { weather_var_val < threshold } 
  threshold_exceedance[is.na(threshold_exceedance)] <- 0
  polygon(x = c(rep(c(Dates[1], Dates), each = 2)), 
          y = c(0, rep(threshold_exceedance, each = 2), 0)* 10000, col = "grey80", border = NA)
  
  # re-draw the 311 records
  lines(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), lty = 1)
  
  # legend and axis
  legend("topleft", legend = agency, bty = 'n')
  legend("topright", fill = "grey80", bty = 'n',
         legend = paste(weather_var_name, ifelse(shade_val_above, ">", "<"), threshold))
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}


# separate traceplots - next 6  agencies
par(mar = c(2,5,0,1), las = 1, mfrow = c(6,1))
for (agency in agencyTypes_collapsed[6:11]) {
  # create empty plot
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
       xlim = range(Dates) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  
  # shade weather variables
  threshold_exceedance <- if (shade_val_above) { 
    weather_var_val > threshold
  } else { weather_var_val < threshold } 
  threshold_exceedance[is.na(threshold_exceedance)] <- 0
  polygon(x = c(rep(c(Dates[1], Dates), each = 2)), 
          y = c(0, rep(threshold_exceedance, each = 2), 0)* 10000, col = "grey80", border = NA)
  
  # re-draw the 311 records
  lines(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), lty = 1)
  
  # legend and axis
  legend("topleft", legend = agency, bty = 'n')
  legend("topright", fill = "grey80", bty = 'n',
         legend = paste(weather_var_name, ifelse(shade_val_above, ">", "<"), threshold))
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}






