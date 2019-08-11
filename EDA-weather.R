rm(list = ls())

####  Load weather dataset #############################################################################

library(data.table)
weather <- data.table::fread("weather_NY_2010_2018Nov.csv")

# simple inspections
names(weather)
head(weather)
tail(weather)

# sort data by date
library(dplyr)
weather <- weather %>%
  arrange(Year, Month, Day)

# create date and unique ID's for each station
weather <- weather %>% 
  mutate(Date = lubridate::ymd(sprintf('%04d%02d%02d', Year, Month, Day)),
         Station_ID = group_indices(., StationName, USAF, WBAN)) %>% 
  mutate(Station_ID = as.character(Station_ID))

# check if there are missing days -- NO
(num_days <- length(unique(weather$Date)))
max(weather$Date) - min(weather$Date)

#### Explore weather station locations ####################################################################

# count the number of unique station locations -- 62 locations, 59 unique names
unique_stations <- weather %>% 
  distinct(Station_ID, StationName, Latitude, Longitude) %>% 
  arrange(StationName)

# count the number of unique stations by day
num_stations_by_date <- weather %>% 
  group_by(Date) %>% 
  summarise(unique_stations = n_distinct(Station_ID, StationName))
# the number of stations is not constant!!

# compare station locations before and after May 2018 
station_ID_after_May2018 <- weather %>% 
  filter(Date == max(Date) - 1) %>%
  pull(Station_ID)
station_ID_before_May2018 <- weather %>% 
  filter(Date == median(Date) + 2) %>%
  pull(Station_ID)
(phased_out_stations_ID <- setdiff(station_ID_before_May2018, station_ID_after_May2018))
distance_matrix <- as.matrix(dist(unique_stations[,c("Latitude", "Longitude")]))
diag(distance_matrix) <- Inf
(new_stations_ID <- setdiff(station_ID_after_May2018, station_ID_before_May2018))
(replaced_stations_ID <- as.character(apply(distance_matrix[new_stations_ID,], 1, which.min)))

# when do the stations report weather info?
report_by_station <- as.data.frame(matrix(nrow = num_days, ncol = nrow(unique_stations)))
colnames(report_by_station) <- unique_stations$Station_ID
for (this_station_ID in as.character(unique_stations$Station_ID)) {
  report_by_this_station <- weather %>%
    group_by(Date) %>%
    summarise(reported = sum(this_station_ID %in% Station_ID))
  report_by_station[this_station_ID] <- report_by_this_station$reported
}

#### Plot the number of stations in use, over time ####

par(las = 1, mar = c(2,3,2,1), mfrow = c(2,1))
plot(num_stations_by_date, ylim = c(30, 55), pch = 20, type = 'b', xaxs="i", xaxt = 'n')
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
abline(v = lubridate::ymd("20180501"), lty = 2)
text(x = lubridate::ymd("20180501"), y = 31, labels = "Structural break in May 2018", pos = 2)
mtext(text = "Number of stations in use", side = 3, at = lubridate::ymd("20100101"), line = 0.5, adj = 0)

par(las = 1, mar = c(2,3,1,1))
report_by_station_to_plot <- report_by_station
image(x = num_stations_by_date$Date, z = as.matrix(report_by_station_to_plot), 
      col = c(NA, "black"), xaxt= "n", yaxt= "n" )
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
mtext(text = "Weather Stations", at = 0.5, 
      side = 2, line = 1, las = 3)

#### Explore data: missingnes pattern at same/similar locations ####################################################################

# mark new stations
report_by_station_to_plot[,new_stations_ID] <- report_by_station_to_plot[,new_stations_ID] * 2
report_by_station_to_plot[,replaced_stations_ID] <- report_by_station_to_plot[,replaced_stations_ID] * 2
# Some have different names, but replaces one another -- 
# CATTARAUGUS CO OLEAN => CATTARAUGUS COUNTY OLEAN AIRPORT
# FLOYD BENNETT MEM => FLOYD BENNETT MEM => FLOYD BENNETT MEMO AIRPORT
# LONG ISLAND MAC ART => LONG ISLAND MAC ART => LONG ISLAND MAC ARTHUR AIRPORT
# CENTRAL PARK => NEW YORK CENTRAL PARK => NYC CENTRAL PARK
exchangeable_stations <- as.character(c(7:8, 16:18, 29:31, c(42,38,37,9)))
report_by_station_to_plot[,exchangeable_stations] <- report_by_station_to_plot[,exchangeable_stations] * 3
# Some have similar names, but operate simultaneously -- 
# MONTAUK & MONTAUK AIRPORT
# ROCHESTER & ROCHESTER
simultaneous_stations <- as.character(c(34:35, 52:53))
report_by_station_to_plot[,simultaneous_stations] <- report_by_station_to_plot[,simultaneous_stations] * 4

# relabel the staion ID's

(relabelled_stations <- list(exchangeable_stations[1:2], # CATTARAUGUS CO OLEAN => CATTARAUGUS COUNTY OLEAN AIRPORT
                             exchangeable_stations[3:5], # FLOYD BENNETT MEM => FLOYD BENNETT MEM => FLOYD BENNETT MEMO AIRPORT
                             exchangeable_stations[6:8], # LONG ISLAND MAC ART => LONG ISLAND MAC ART => LONG ISLAND MAC ARTHUR AIRPORT
                             exchangeable_stations[9:12], # CENTRAL PARK => NEW YORK CITY  CENTRAL PARK => NEW YORK CENTRAL PARK => NYC CENTRAL PARK
                             c(replaced_stations_ID[1], new_stations_ID[1]), # OGDENSBURG => OGDENSBURG INTL 
                             c(replaced_stations_ID[2], new_stations_ID[2]))) # THE BATTERY => PORT AUTH DOWNTN MANHATTAN WALL ST HEL

# when do the relabelled stations report weather info?
report_by_relabelled_station <- as.data.frame(matrix(nrow = num_days, 
                                                     ncol = length(unlist(relabelled_stations))))
colnames(report_by_relabelled_station) <- unlist(relabelled_stations)
for (this_station_ID in unlist(relabelled_stations)) {
  report_by_this_station <- weather %>%
    group_by(Date) %>%
    summarise(reported = sum(this_station_ID %in% Station_ID))
  report_by_relabelled_station[this_station_ID] <- report_by_this_station$reported
}

# plotting all stations over time

par(las = 1, mar = c(2,3,2,1), mfrow = c(2,1))
report_by_station_to_plot <- report_by_station
# mark new stations
report_by_station_to_plot[,new_stations_ID] <- report_by_station_to_plot[,new_stations_ID] * 2
report_by_station_to_plot[,replaced_stations_ID] <- report_by_station_to_plot[,replaced_stations_ID] * 2
report_by_station_to_plot[,exchangeable_stations] <- report_by_station_to_plot[,exchangeable_stations] * 3
report_by_station_to_plot[,simultaneous_stations] <- report_by_station_to_plot[,simultaneous_stations] * 4
image(x = num_stations_by_date$Date, z = as.matrix(report_by_station_to_plot), 
      col = c(NA, "black", "red", "red", "blue"), xaxt= "n", yaxt= "n" )
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
mtext(text = "Weather Stations", at = 0.5, 
      side = 2, line = 1, las = 3)
mtext("Same location, different names", at = lubridate::ymd("20100101"), 
      side = 3, adj = 0, line = 0.5, col = c("red")) 
mtext("Same/similar names, different locations", at = lubridate::ymd("20181112"), 
      side = 3, adj = 1, line = 0.5, col = c("blue")) 

# focus on replaced stations

par(las = 1, mar = c(2,3,2,1))
report_by_station_to_plot <- report_by_relabelled_station
report_by_station_to_plot[unlist(relabelled_stations[seq(2,6,2)])] <- 
  report_by_station_to_plot[unlist(relabelled_stations[seq(2,6,2)])] * 2
image(x = num_stations_by_date$Date, z = as.matrix(report_by_station_to_plot), 
      col = c(NA, grey(0.4), grey(0.7)), xaxt= "n", yaxt= "n" )
text(x = lubridate::ymd("20160101"), y = 0.6, labels = "Central Park station operated under four names...")
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
mtext(text = "Weather Stations", at = 0.5, 
      side = 2, line = 1, las = 3)
mtext("Same location, different names", at = lubridate::ymd("20100101"), 
      side = 3, adj = 0, line = 0.5) 

#### Explore a pair of stations nearby with one phasing out ####################################################################

# THE BATTERY => PORT AUTH DOWNTN MANHATTAN WALL ST HEL
relabelled_stations_pair_index <- 6

test <- vector("list", 2)
names(test) <- relabelled_stations[[relabelled_stations_pair_index]]
# names(test) <- c("26", "50")
for (this_station_ID in names(test)) {
  report_by_this_station <- weather %>%
    group_by(Date) %>%
    filter(Station_ID == this_station_ID) 
  test[[this_station_ID]] <- report_by_this_station
}

station_old <- test[[names(test)[1]]] # outgoing station
station_new <- test[[names(test)[2]]] # new station

## Both report temperatures

par(mfrow = c(3,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# temperatures -- MeanTemp
plot(x = station_old$Date, y = station_old$MeanTemp, xlim = range(weather$Date), pch = 20)
points(x = station_new$Date, y = station_new$MeanTemp, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 70, labels = "MeanTemp", adj = 0)
# temperatures -- MaxTemp
plot(x = station_old$Date, y = station_old$MaxTemp, xlim = range(weather$Date), pch = 20)
points(x = station_new$Date, y = station_new$MaxTemp, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 80, labels = "MaxTemp", adj = 0)
# temperatures -- MinTemp
plot(x = station_old$Date, y = station_old$MinTemp, xlim = range(weather$Date), pch = 20)
points(x = station_new$Date, y = station_new$MinTemp, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 60, labels = "MinTemp", adj = 0)

# Old staions have zero measurements

par(mfrow = c(4,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# Windspeed
plot(x = station_old$Date, y = station_old$WindSpeed, 
     xlim = range(weather$Date), ylim = range(c(0, station_new$WindSpeed), na.rm = T), pch = 20)
points(x = station_new$Date, y = station_new$WindSpeed, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = max(station_new$WindSpeed, na.rm = T)*0.8, labels = "WindSpeed", adj = 0)
# Percipitation
plot(x = station_old$Date, y = station_old$Percipitation, 
     xlim = range(weather$Date), ylim = range(station_new$Percipitation, na.rm = T), pch = 20)
points(x = station_new$Date, y = station_new$Percipitation, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 80, labels = "Percipitation", adj = 0)
# Rain
plot(x = station_old$Date, y = station_old$Rain, 
     xlim = range(weather$Date), ylim = range(station_new$Rain, na.rm = T), pch = 20)
points(x = station_new$Date, y = station_new$Rain, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 0.8, labels = "Rain", adj = 0)
# SnowIce
plot(x = station_old$Date, y = station_old$SnowIce, 
     xlim = range(weather$Date), ylim = range(station_new$SnowIce, na.rm = T), pch = 20)
points(x = station_new$Date, y = station_new$SnowIce, pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 0.8, labels = "SnowIce", adj = 0)

# Old staions do not have measurements

par(mfrow = c(3,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# DewPoint
plot(x = station_new$Date, y = station_new$DewPoint, 
     xlim = range(weather$Date), pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 56, labels = "DewPoint", adj = 0)
# MaxSustainedWind
plot(x = station_new$Date, y = station_new$MaxSustainedWind, 
     xlim = range(weather$Date), pch = 20, col = 2)
text(x = min(weather$Date)+50, y = max(station_new$MaxSustainedWind, na.rm = T)*0.8, labels = "MaxSustainedWind", adj = 0)
# Gust
plot(x = station_new$Date, y = station_new$Gust,
     xlim = range(weather$Date), pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 40, labels = "Gust", adj = 0)

# SnowDepth does not exist for both!!

#### Data quality is a monir issue at central park stations ####################################################################

test <- vector("list", length(relabelled_stations[[4]]))
names(test) <- relabelled_stations[[4]]
for (this_station_ID in relabelled_stations[[4]]) {
  report_by_this_station <- weather %>%
    group_by(Date) %>%
    filter(Station_ID == this_station_ID) 
  test[[this_station_ID]] <- report_by_this_station
}

station_42 <- test[['42']]
station_38 <- test[['38']]
station_37 <- test[['37']]
station_9 <- test[['9']]

## All report temperatures

#png("NYC_CentralPark_tmp.png", width = 900, height = 350)
par(mfrow = c(3,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# temperatures -- MeanTemp
plot(x = station_42$Date, y = station_42$MeanTemp, xlim = range(weather$Date), pch = 20)
points(x = station_37$Date, y = station_37$MeanTemp, col = 3, pch = 20)
points(x = station_9$Date, y = station_9$MeanTemp, col = 4, pch = 20)
points(x = station_38$Date, y = station_38$MeanTemp, col = 2, pch = 20)
text(x = min(weather$Date)+300, y = 87, labels = "MeanTemp", adj = 0)
# temperatures -- MaxTemp
plot(x = station_42$Date, y = station_42$MaxTemp, xlim = range(weather$Date), pch = 20)
points(x = station_37$Date, y = station_37$MaxTemp, col = 3, pch = 20)
points(x = station_9$Date, y = station_9$MaxTemp, col = 4, pch = 20)
points(x = station_38$Date, y = station_38$MaxTemp, col = 2, pch = 20)
text(x = min(weather$Date)+300, y = 100, labels = "MaxTemp", adj = 0)
# temperatures -- MinTemp
plot(x = station_42$Date, y = station_42$MinTemp, xlim = range(weather$Date), pch = 20)
points(x = station_37$Date, y = station_37$MinTemp, col = 3, pch = 20)
points(x = station_9$Date, y = station_9$MinTemp, col = 4, pch = 20)
points(x = station_38$Date, y = station_38$MinTemp, col = 2, pch = 20)
text(x = min(weather$Date)+300, y = 80, labels = "MinTemp", adj = 0)
#dev.off()


# All staions have non-zero measurements

#png("NYC_CentralPark_rain.png", width = 900, height = 350)
par(mfrow = c(4,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# Windspeed
plot(x = station_42$Date, y = station_42$WindSpeed, 
     xlim = range(weather$Date), ylim = range(station_42$WindSpeed, na.rm = T), pch = 20)
points(x = station_37$Date, y = station_37$WindSpeed, col = 3, pch = 20)
points(x = station_9$Date, y = station_9$WindSpeed, col = 4, pch = 20)
points(x = station_38$Date, y = station_38$WindSpeed, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 15, labels = "WindSpeed", adj = 0)
# Percipitation
plot(x = station_42$Date, y = station_42$Percipitation, 
     xlim = range(weather$Date), ylim = range(station_42$Percipitation, na.rm = T), pch = 20)
points(x = station_37$Date, y = station_37$Percipitation, col = 3, pch = 20)
points(x = station_9$Date, y = station_9$Percipitation, col = 4, pch = 20)
points(x = station_38$Date, y = station_38$Percipitation, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 3, labels = "Percipitation", adj = 0)
# Rain
plot(x = station_42$Date, y = station_42$Rain, 
     xlim = range(weather$Date), ylim = range(station_42$Rain, na.rm = T), pch = 20)
points(x = station_37$Date, y = station_37$Rain, col = 3, pch = 20)
points(x = station_9$Date, y = station_9$Rain, col = 4, pch = 20)
points(x = station_38$Date, y = station_38$Rain, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 0.8, labels = "Rain", adj = 0)
# SnowIce
plot(x = station_42$Date, y = station_42$SnowIce, 
     xlim = range(weather$Date), ylim = range(station_42$SnowIce, na.rm = T), pch = 20)
points(x = station_37$Date, y = station_37$SnowIce, pch = 20, col = 3)
points(x = station_9$Date, y = station_9$SnowIce, pch = 20, col = 4)
points(x = station_38$Date, y = station_38$SnowIce, pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 0.8, labels = "SnowIce", adj = 0)
#dev.off()


# Old staions do not have measurements

#png("NYC_CentralPark_wind.png", width = 900, height = 350)
par(mfrow = c(4,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# DewPoint
plot(x = station_42$Date, y = station_42$DewPoint, 
     xlim = range(weather$Date), pch = 20)
points(x = station_37$Date, y = station_37$DewPoint, pch = 20, col = 3)
points(x = station_9$Date, y = station_9$DewPoint, pch = 20, col = 4)
points(x = station_38$Date, y = station_38$DewPoint, pch = 20, col = 2)
text(x = min(weather$Date)+300, y = 65, labels = "DewPoint", adj = 0)
# MaxSustainedWind
plot(x = station_42$Date, y = station_42$MaxSustainedWind, 
     xlim = range(weather$Date), ylim = c(5,30), pch = 20, col = 1)
points(x = station_37$Date, y = station_37$MaxSustainedWind, pch = 20, col = 3)
points(x = station_9$Date, y = station_9$MaxSustainedWind, pch = 20, col = 4)
points(x = station_38$Date, y = station_38$MaxSustainedWind, pch = 20, col = 2)
text(x = min(weather$Date)+300, y = 25, labels = "MaxSustainedWind", adj = 0)
# Gust
plot(x = station_42$Date, y = station_42$Gust, 
     xlim = range(weather$Date), pch = 20, col = 1)
points(x = station_37$Date, y = station_37$Gust, pch = 20, col = 3)
points(x = station_9$Date, y = station_9$Gust, pch = 20, col = 4)
points(x = station_38$Date, y = station_38$Gust, pch = 20, col = 2)
text(x = min(weather$Date)+300, y = 40, labels = "Gust", adj = 0)
# SnowDepth
plot(x = station_42$Date, y = station_42$SnowDepth, 
     xlim = range(weather$Date), pch = 20, col = 1)
points(x = station_37$Date, y = station_37$SnowDepth, pch = 20, col = 3)
points(x = station_9$Date, y = station_9$SnowDepth, pch = 20, col = 4)
points(x = station_38$Date, y = station_38$SnowDepth, pch = 20, col = 2)
text(x = min(weather$Date)+300, y = 16, labels = "SnowDepth", adj = 0)
#dev.off()

#### Focus on NYC and Combine stations at the same location ####################################################################

# list of stations in NYC
NYC_stations <- list("4", c("59", "50"), c("42", "38", "37", "9"), "28", "27", "26")
# NYC_stations <- unique_stations %>% filter(Latitude > 40.5 & Latitude < 41 & Longitude > -74.5 & Longitude < -73.5) %>% pull(Station_ID)

# extract stations in NYC
weather_NYC <- weather %>%
  filter(Station_ID %in% unlist(NYC_stations))

weather_NYC %>% distinct(Station_ID)
names(weather_NYC)

##### convert long to wide format ################################################################################

library(tidyr)

MeanTemp_wide <- weather_NYC %>% select(Date, Station_ID, MeanTemp) %>% spread(Station_ID, MeanTemp)
MinTemp_wide <- weather_NYC %>% select(Date, Station_ID, MinTemp) %>% spread(Station_ID, MinTemp)
MaxTemp_wide <- weather_NYC %>% select(Date, Station_ID, MaxTemp) %>% spread(Station_ID, MaxTemp)
DewPoint_wide <- weather_NYC %>% select(Date, Station_ID, DewPoint) %>% spread(Station_ID, DewPoint)
Percipitation_wide <- weather_NYC %>% select(Date, Station_ID, Percipitation) %>% spread(Station_ID, Percipitation)
WindSpeed_wide <- weather_NYC %>% select(Date, Station_ID, WindSpeed) %>% spread(Station_ID, WindSpeed)
MaxSustainedWind_wide <- weather_NYC %>% select(Date, Station_ID, MaxSustainedWind) %>% spread(Station_ID, MaxSustainedWind)
Gust_wide <- weather_NYC %>% select(Date, Station_ID, Gust) %>% spread(Station_ID, Gust)
Rain_wide <- weather_NYC %>% select(Date, Station_ID, Rain) %>% spread(Station_ID, Rain)
SnowDepth_wide <- weather_NYC %>% select(Date, Station_ID, SnowDepth) %>% spread(Station_ID, SnowDepth)
SnowIce_wide <- weather_NYC %>% select(Date, Station_ID, SnowIce) %>% spread(Station_ID, SnowIce)

############# replace artificial zeros with NA's #################################################################

check_variable <- SnowDepth_wide
par(mfrow = c(10, 1))
for (i in unlist(NYC_stations)) {
  if (sum(!is.na(check_variable[,i])) >0) {
    plot(check_variable[,i])
  } else {
    plot(x = check_variable$Date, y = rep(0, nrow(check_variable)), col = 2)
  }
  legend("topleft", legend = i)
}

# MeanTemp_wide is fine
# MinTemp_wide is fine
# MaxTemp_wide is fine
# DewPoint_wide is fine (NA: 4, 59, 27)
# Percipitation_wide: 4, 59, 27 (also: weird 100's)
Percipitation_wide[,c("4", "59", "27")] <- NA
# WindSpeed_wide: 4, 59
WindSpeed_wide[,c("4", "59")] <- NA
# MaxSustainedWind_wide is fine (NA: 59)
# Gust_wide is fine (NA: 4, 59, 27)
# Rain_wide: 4, 59, 27
Rain_wide[,c("4", "59", "27")] <- NA
# SnowDepth_wide is fine (but INTERESTING!)
# SnowIce_wide: 4, 59, 27
SnowIce_wide[,c("4", "59", "27")] <- NA

############## now combine similar locations ###############################################################

NYC_stations
(NYC_stations_combined <- paste0('Station.', c("4", "50", "9", "28", "27", "26")))

MeanTemp_wide <- MeanTemp_wide %>% 
  mutate('9' = coalesce(!!!MeanTemp_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!MeanTemp_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

MinTemp_wide <- MinTemp_wide %>% 
  mutate('9' = coalesce(!!!MinTemp_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!MinTemp_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

MaxTemp_wide <- MaxTemp_wide %>% 
  mutate('9' = coalesce(!!!MaxTemp_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!MaxTemp_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

DewPoint_wide <- DewPoint_wide %>% 
  mutate('9' = coalesce(!!!DewPoint_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!DewPoint_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

Percipitation_wide <- Percipitation_wide %>% 
  mutate('9' = coalesce(!!!Percipitation_wide[rev(NYC_stations[[3]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

WindSpeed_wide <- WindSpeed_wide %>% 
  mutate('9' = coalesce(!!!WindSpeed_wide[rev(NYC_stations[[3]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

MaxSustainedWind_wide <- MaxSustainedWind_wide %>% 
  mutate('9' = coalesce(!!!MaxSustainedWind_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!MaxSustainedWind_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

Gust_wide <- Gust_wide %>% 
  mutate('9' = coalesce(!!!Gust_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!Gust_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

Rain_wide <- Rain_wide %>% 
  mutate('9' = coalesce(!!!Rain_wide[rev(NYC_stations[[3]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

SnowDepth_wide <- SnowDepth_wide %>% 
  mutate('9' = coalesce(!!!SnowDepth_wide[rev(NYC_stations[[3]])])) %>% 
  mutate('50' = coalesce(!!!SnowDepth_wide[rev(NYC_stations[[2]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))

SnowIce_wide <- SnowIce_wide %>% 
  mutate('9' = coalesce(!!!SnowIce_wide[rev(NYC_stations[[3]])])) %>% 
  select(-c("42", "38", "37", "59")) %>% 
  setNames(paste0(c("", rep('Station.', ncol(.)-1)), names(.))) %>%
  mutate(Average = rowMeans(select(., starts_with("Station")), na.rm = TRUE))


weather_NYC_by_var_wide <- list(MeanTemp = MeanTemp_wide, MinTemp = MinTemp_wide, MaxTemp = MaxTemp_wide, 
                                DewPoint = DewPoint_wide, Percipitation = Percipitation_wide, WindSpeed = WindSpeed_wide, 
                                MaxSustainedWind = MaxSustainedWind_wide, Gust = Gust_wide, Rain = Rain_wide, 
                                SnowDepth = SnowDepth_wide, SnowIce = SnowIce_wide)


save(weather_NYC_by_var_wide, file = "weather_NYC.Rdata", version = 2)
load("weather_NYC.Rdata")

#### check if combined data looks OK ######################################################################

# MeanTemp_wide MinTemp_wide MaxTemp_wide DewPoint_wide Percipitation_wide
# WindSpeed_wide MaxSustainedWind_wide Gust_wide Rain_wide SnowDepth_wide SnowIce_wide 

NYC_stations_combined

check_variable <- SnowIce_wide
par(mfrow = c(6, 1), mar = c(3,3,1,1))
for (i in NYC_stations_combined) {
  if (sum(!is.na(check_variable[,i])) >0) {
    plot(check_variable[,i])
  } else {
    plot(x = check_variable$Date, y = rep(0, nrow(check_variable)), col = 2)
  }
  legend("topleft", legend = i)
}

#### visualize cleaned data #################################################################################

# when do the NYC stations report weather info?
report_by_NYC_station <- as.data.frame(matrix(nrow = num_days, 
                                              ncol = length(unlist(NYC_stations))))
colnames(report_by_NYC_station) <- unlist(NYC_stations)
for (this_station_ID in unlist(NYC_stations)) {
  report_by_this_station <- weather_NYC %>%
    group_by(Date) %>%
    summarise(reported = sum(this_station_ID %in% Station_ID))
  report_by_NYC_station[this_station_ID] <- report_by_this_station$reported
}

# before / after data merge in NYC locations

par(las = 1, mar = c(2,3,2,1), mfrow = c(2,1))
report_by_station_to_plot <- report_by_NYC_station
report_by_station_to_plot[unlist(NYC_stations[seq(2,6,2)])] <- 
  report_by_station_to_plot[unlist(NYC_stations[seq(2,6,2)])] * 2
image(x = num_stations_by_date$Date, z = as.matrix(report_by_station_to_plot), 
      col = c(NA, grey(0.4), grey(0.7)), xaxt= "n", yaxt= "n" )
text(x = lubridate::ymd("20160101"), y = 0.5, labels = "Central Park station operated under four names...")
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
mtext(text = "Weather Stations", at = 0.5, 
      side = 2, line = 1, las = 3)
mtext("Same location, different names", at = lubridate::ymd("20100101"), 
      side = 3, adj = 0, line = 0.5) 

par(las = 1, mar = c(2,3,3,1))
report_by_station_to_plot <- !is.na(MeanTemp_wide[NYC_stations_combined])
report_by_station_to_plot[,NYC_stations_combined[seq(2,6,2)]] <- 
  report_by_station_to_plot[,NYC_stations_combined[seq(2,6,2)]] * 2
image(x = num_stations_by_date$Date, z = as.matrix(report_by_station_to_plot), 
      col = c(NA, grey(0.4), grey(0.7)), xaxt= "n", yaxt= "n" )
text(x = lubridate::ymd("20160101"), y = 0.40, labels = "All records at Central Park combined", col = "white")
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
mtext(text = "Weather Stations", at = 0.5, 
      side = 2, line = 1, las = 3)
mtext("... now grouped, cleand, combined", at = lubridate::ymd("20100101"), 
      side = 3, adj = 0, line = 0.5) 

#### Explore spatial patterns ####################################################################

library(maps)
library(mapdata)
library(mapproj)

# compare station locations before and after May 2018 
par(mfrow = c(1,1), mar = c(1,1,1,1))
xy <- map("county", xlim = c(-80, -71), ylim = c(40, 45), resolution = 0)
map_color <- rep(NA, nrow(unique_stations))
map_color[as.numeric(station_ID_after_May2018)] <- "black"
map_color[as.numeric(setdiff(station_ID_before_May2018, replaced_stations_ID))] <- "red"
map_color[as.numeric(intersect(station_ID_after_May2018, station_ID_before_May2018))] <- "black"
with(unique_stations, points(Longitude, Latitude, col = map_color, cex = 1.5, pch = 20))
legend("topleft", pch = 20, pt.cex = 1.5, bg = grey(0.8), box.lwd = 0,
       col = c("red"), legend = c("phased out after May 2018"))

# zoom-in of NY city
par(mfrow = c(1,1), mar = c(1,1,1,1))
xy <- map("county", xlim = c(-74.5, -73.5), ylim = c(40.5, 41), resolution = 0)
with(unique_stations, points(Longitude, Latitude, col = map_color, cex = 1.5, pch = 20))
legend("bottomright", pch = 20, pt.cex = 1.5, bg = grey(0.8), box.lwd = 0,
       col = c("red"), legend = c("phased out after May 2018"))

plotly::plot_ly(data = unique_stations, x = ~Longitude, y = ~Latitude, mode = 'markers',
                hoverinfo = 'text', text = ~paste('Station ID: ', Station_ID))

