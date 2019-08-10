rm(list = ls())

####  Load weather dataset ####

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

#### Explore weather station locations ####

# count the number of unique station locations -- 62 locations, 59 unique names
unique_stations <- weather %>% 
  distinct(Station_ID, StationName, Latitude, Longitude) %>% 
  arrange(StationName)

# count the number of unique stations by day
num_stations_by_date <- weather %>% 
  group_by(Date) %>% 
  summarise(unique_stations = n_distinct(Station_ID, StationName))
# the number of stations is not constant!

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

#### Cleaning data: combine stations at same/similar locations ####

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
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
mtext(text = "Weather Stations", at = 0.5, 
      side = 2, line = 1, las = 3)
mtext("Same location, different names", at = lubridate::ymd("20100101"), 
      side = 3, adj = 0, line = 0.5) 

#### Explore a pair of stations replacement ####

test <- vector("list", length(relabelled_stations[[5]]))
names(test) <- relabelled_stations[[5]]
for (this_station_ID in relabelled_stations[[5]]) {
  report_by_this_station <- weather %>%
    group_by(Date) %>%
    filter(Station_ID == this_station_ID) 
  test[[this_station_ID]] <- report_by_this_station
}

station_43 <- test[['43']]
station_44 <- test[['44']]

## Both report temperatures

par(mfrow = c(3,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# temperatures -- MeanTemp
plot(x = station_43$Date, y = station_43$MeanTemp, xlim = range(weather$Date), pch = 20)
points(x = station_44$Date, y = station_44$MeanTemp, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 70, labels = "MeanTemp", adj = 0)
# temperatures -- MaxTemp
plot(x = station_43$Date, y = station_43$MaxTemp, xlim = range(weather$Date), pch = 20)
points(x = station_44$Date, y = station_44$MaxTemp, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 80, labels = "MaxTemp", adj = 0)
# temperatures -- MinTemp
plot(x = station_43$Date, y = station_43$MinTemp, xlim = range(weather$Date), pch = 20)
points(x = station_44$Date, y = station_44$MinTemp, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 60, labels = "MinTemp", adj = 0)

# Old staions have zero measurements

par(mfrow = c(4,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# Windspeed
plot(x = station_43$Date, y = station_43$WindSpeed, 
     xlim = range(weather$Date), ylim = range(station_44$WindSpeed, na.rm = T), pch = 20)
points(x = station_44$Date, y = station_44$WindSpeed, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 12, labels = "WindSpeed", adj = 0)
# Percipitation
plot(x = station_43$Date, y = station_43$Percipitation, 
     xlim = range(weather$Date), ylim = range(station_44$Percipitation, na.rm = T), pch = 20)
points(x = station_44$Date, y = station_44$Percipitation, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 80, labels = "Percipitation", adj = 0)
# Rain
plot(x = station_43$Date, y = station_43$Rain, 
     xlim = range(weather$Date), ylim = range(station_44$Rain, na.rm = T), pch = 20)
points(x = station_44$Date, y = station_44$Rain, col = 2, pch = 20)
text(x = min(weather$Date)+50, y = 0.8, labels = "Rain", adj = 0)
# SnowIce
plot(x = station_43$Date, y = station_43$SnowIce, 
     xlim = range(weather$Date), ylim = range(station_44$SnowIce, na.rm = T), pch = 20)
points(x = station_44$Date, y = station_44$SnowIce, pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 0.8, labels = "SnowIce", adj = 0)

# Old staions do not have measurements

par(mfrow = c(3,1), mar = c(2,3,1,1), xaxs="i", las = 1)
# DewPoint
plot(x = station_44$Date, y = station_44$DewPoint, 
     xlim = range(weather$Date), pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 56, labels = "DewPoint", adj = 0)
# MaxSustainedWind
plot(x = station_44$Date, y = station_44$MaxSustainedWind, 
     xlim = range(weather$Date), pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 28, labels = "MaxSustainedWind", adj = 0)
# Gust
plot(x = station_44$Date, y = station_44$Gust, 
     xlim = range(weather$Date), pch = 20, col = 2)
text(x = min(weather$Date)+50, y = 40, labels = "Gust", adj = 0)

# SnowDepth does not exist for both!!


#### Explore spatial patterns ####

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

