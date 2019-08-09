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

# create date column
weather <- weather %>% 
  mutate(Date = lubridate::ymd(sprintf('%04d%02d%02d', Year, Month, Day)),
         Station_ID = group_indices(., StationName, USAF, WBAN))

# check if there are missing days -- NO
num_days <- length(unique(weather$Date))
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
phased_out_stations_ID <- setdiff(station_ID_before_May2018, station_ID_after_May2018)
distance_matrix <- as.matrix(dist(unique_stations[,c("Latitude", "Longitude")]))
diag(distance_matrix) <- Inf
new_stations_ID <- setdiff(station_ID_after_May2018, station_ID_before_May2018)
replaced_stations_ID <- apply(distance_matrix[new_stations_ID,], 1, which.min)

# when do the stations report weather info?
report_by_station <- as.data.frame(matrix(nrow = num_days, ncol = nrow(unique_stations)))
colnames(report_by_station) <- unique_stations$Station_ID
for (this_station_ID in unique_stations$Station_ID) {
  report_by_this_station <- weather %>%
    group_by(Date) %>%
    summarise(reported = sum(this_station_ID %in% Station_ID))
  report_by_station[this_station_ID] <- report_by_this_station$reported
}

#### Plot the number of stations in use, over time ####

par(las = 1, mar = c(3,3,2,1), mfrow = c(2,1))
plot(num_stations_by_date, ylim = c(30, 55), pch = 20, type = 'b', xaxs="i", xaxt = 'n')
axis(side = 1, labels = seq(2010, 2020, 2), at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")))
abline(v = lubridate::ymd("20180501"), lty = 2)
text(x = lubridate::ymd("20180501"), y = 31, labels = "Structural break in May 2018", pos = 2)
mtext(text = "Number of stations in use", side = 3, at = lubridate::ymd("20100101"), line = 0.5, adj = 0)

par(las = 1, mar = c(2,3,1,1))
report_by_station_to_plot <- report_by_station
# mark new stations
report_by_station_to_plot[,new_stations_ID] <- report_by_station_to_plot[,new_stations_ID] * 2
report_by_station_to_plot[,replaced_stations_ID] <- report_by_station_to_plot[,replaced_stations_ID] * 2
# Some have different names, but replaces one another -- 
# CATTARAUGUS CO OLEAN & CATTARAUGUS COUNTY OLEAN AIRPORT
# FLOYD BENNETT MEM & FLOYD BENNETT MEMO AIRPORT
# LONG ISLAND MAC ART & LONG ISLAND MAC ARTHUR AIRPORT
# CENTRAL PARK & NEW YORK CENTRAL PARK & NYC CENTRAL PARK
exchangeable_stations <- c(7:8, 16:18, 29:31, c(42,37,9))
report_by_station_to_plot[,exchangeable_stations] <- report_by_station_to_plot[,exchangeable_stations] * 3
# Some have similar names, but operate simultaneously -- 
# MONTAUK & MONTAUK AIRPORT
# ROCHESTER & ROCHESTER
simultaneous_stations <- c(34:35, 52:53)      
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

#### Clean data: combine stations at same/similar locations ####



#### Explore spatial patterns ####

library(maps)
library(mapdata)
library(mapproj)

# compare station locations before and after May 2018 
par(mfrow = c(1,1), mar = c(1,1,1,1))
xy <- map("county", xlim = c(-80, -71), ylim = c(40, 45), resolution = 0)
map_color <- rep(NA, nrow(unique_stations))
map_color[station_ID_after_May2018] <- "black"
map_color[setdiff(station_ID_before_May2018, replaced_stations_ID)] <- "red"
map_color[intersect(station_ID_after_May2018, station_ID_before_May2018)] <- "black"
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

