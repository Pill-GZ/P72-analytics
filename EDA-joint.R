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
par(mar = c(2,4,1,1), las = 1, mfrow = c(1,1))
plot(x = tally_by_date_agency_wide$Date, y = rowSums(tally_by_date_agency_wide[,-1]), 
     xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
     type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
# legend("topleft", legend = agency, bty = 'n')
text(lubridate::ymd("20110101"), y = 11500, adj = 0,
     labels = "Aggregate number of calls / day reaveals an upward drift ...")
text(lubridate::ymd("20160101"), y = 3000, adj = 0,
     labels = "... maybe some cyclical patterns ...")
text(lubridate::ymd("20130101"), y = 1500, adj = 0,
     labels = "... nothing much else.")
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
  if (weather_var_name == weather_var_names[5]) {
    plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'l', ylim = c(0, 3))
  } else {
    plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'l')
  }
  legend("topleft", legend = weather_var_name)
}

# plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'b', log = 'y',  ylim = c(0.0025, 2))

par(mar = c(3,3,1,1), mfrow = c(6,1))
for (weather_var_name in weather_var_names[6:11]) {
  plot(weather_NYC_by_var_wide %>% pluck(weather_var_name, 'Average'), type = 'l')
  legend("topleft", legend = weather_var_name)
}

# temp fluctuation within a day
par(mfrow = c(1,1))
plot(weather_NYC_by_var_wide %>% pluck('MaxTemp', 'Average') - 
       weather_NYC_by_var_wide %>% pluck('MinTemp', 'Average'), type = 'l')


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

## traceplot for DOT calls -- shaded by SnowDepth ##########################################################
agency <- "DOT"
# create empty plot
par(mfrow = c(1,1), mar = c(2,4,1,1), las = 1)
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

## traceplot for DOT calls -- shaded by SnowDepth zoom in ####################################################

par(mfrow = c(1,1), mar = c(2,4,1,1), las = 1)
plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
     xlim = lubridate::ymd(c("20150101", "20151231")), # ylim = c(20, 10000),
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
text(x = lubridate::ymd(c("20150415")), y = 2300, adj = 0,
     labels = "Snow depth is a leading, strong, (very likely) causal signal ... ")
text(x = lubridate::ymd(c("20150615")), y = 2100, adj = 0,
     labels = "... whose effect accumulates, and dampens over time")
legend("topright", legend = agency, bty = 'n', lty = 1)
legend("topleft", fill = "grey80", bty = 'n',
       legend = paste(weather_var_name, ifelse(shade_val_above, ">", "<"), threshold))
axis(side = 1, labels = month.abb,
     at = lubridate::ymd(c(paste0("20150", 1:9, "01"), paste0("2015", 10:12, "01"))))

## feature engineering illustration -- pulse response intervention #########################################

pulse <- c(rep(0, 100), rep(1, 200), rep(0, 700))
response <- exp(-(0:900)/200)
par(mar = c(2,3,1,1), mfrow = c(3,1), las = 1, cex = 1)
plot(c(rep(0, 99), response), type = 'l', ylim = c(0, 1.5), xaxt = 'n', col = "blue", lwd = 2)
axis(side = 1, at = seq(0, 1000, 100), labels = seq(-1, 9, 1))
text(x = 400, y = 1.2, labels = "Response filter for a single pulse f")
text(x = 530, y = 0.9, labels = "... i.e., #calls caused by one snowy day")
plot(pulse, type = 'l', ylim = c(0, 1.5), xaxt = 'n', col = "red", lwd = 2)
axis(side = 1, at = seq(0, 1000, 100), labels = seq(-1, 9, 1))
text(x = 500, y = 1.2, labels = "Signal sustained over time x")
text(x = 600, y = 0.9, labels = "... i.e., consecutive snowy days")
response <- c(rep(0, 99), exp(-(0:900)/200))
convolution <- rep(0, 1000)
for (i in 1:200) { convolution <- convolution + lag(response, default = 0, n = i) }
plot(convolution/100, type = 'l', ylim = c(0,1.5), xaxt = 'n', col = "purple", lwd = 2)
axis(side = 1, at = seq(0, 1000, 100), labels = seq(-1, 9, 1))
text(x = 600, y = 1.3, labels = "Observed values y(t)")
text(x = 670, y = 1.0, labels = "... i.e., total number of calls")
text(x = 720, y = 0.7, labels = "... is their convolution: y = f*x")
