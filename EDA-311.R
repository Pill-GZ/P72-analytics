rm(list = ls())

####  Load weather dataset ####

# install.packages('bit64')
library(data.table)
records <- data.table::fread("311_Service_Requests_from_2010_to_Present.csv")
# 
colnames(records) <- make.names(colnames(records), unique = TRUE)

## complaint type

complaint_type_summary <- summary(factor(records$Complaint.Type))
plot(complaint_type_summary)

complaint_type_table <- table(factor(records$Complaint.Type))
complaint_type_table <- sort(complaint_type_table, decreasing = T)
length(complaint_type_table)

par(mar = c(15,3,1,1), mfrow = c(1,1))
plot(complaint_type_table, las = 3)
par(mar = c(3,3,1,1), mfrow = c(1,1))
plot(cumsum(complaint_type_table) / sum(complaint_type_table), ylim = c(0,1))
abline(h = 0.95, lty = 2)

library(dplyr)

records %>% 
  filter(Complaint.Type == './validate_form.php')

## descriptor

descriptor_summary <- summary(factor(records$Descriptor))
plot(descriptor_summary)

descriptor_table <- table(factor(records$Descriptor))
descriptor_table <- sort(descriptor_table, decreasing = T)
length(descriptor_table)

par(mar = c(15,3,1,1))
plot(descriptor_table, las = 3)

plot(cumsum(descriptor_table) / sum(descriptor_table), ylim = c(0,1))
abline(h = 0.95, lty = 2)

## agency

agency_summary <- summary(factor(records$Agency))
plot(agency_summary)

agency_table <- table(factor(records$Agency))
agency_table <- sort(agency_table, decreasing = T)
length(agency_table)

par(mar = c(5,5,1,1))
plot(agency_table, las = 2)

barplot(cumsum(agency_table) / sum(agency_table), ylim = c(0,1), las = 2)
abline(h = 0.95, lty = 2)

test <- records %>% 
  group_by(Agency, Agency.Name) %>% 
  summarise(n_records = n_distinct(Unique.Key)) %>%
  arrange(desc(n_records))

test <- test %>% 
  group_by(Agency) %>%
  mutate(total_records_by_agency = sum(n_records)) %>%
  arrange(desc(total_records_by_agency))

test %>% distinct(Agency)
# Top 10 hadling agencies
# 1 HPD   
# 2 NYPD  
# 3 DOT   
# 4 DSNY  
# 5 DEP   
# 6 DOB   
# 7 DPR   
# 8 DOHMH 
# 9 DOF   
# 10 TLC

# locations

location_table <- table(factor(records$Park.Borough))
location_table <- sort(location_table, decreasing = T)
length(location_table)

par(mar = c(10,5,1,1))
plot(location_table, las = 2)

records_sample <- sample_n(records, size = 10000)
fwrite(x = records_sample, file = "311_records_sample_n10000.csv")

records_sample <- sample_n(records, size = 100000)
fwrite(x = records_sample, file = "311_records_sample_n100000.csv")


library(maps)
library(mapdata)
library(mapproj)

# compare station locations before and after May 2018 
par(mfrow = c(1,1), mar = c(1,1,1,1))
xy <- map("county", xlim = c(-80, -71), ylim = c(40, 45), resolution = 0)
map_color <- rep("black", nrow(records_sample))
# map_color[station_ID_after_May2018] <- "black"
# map_color[setdiff(station_ID_before_May2018, replaced_stations_ID)] <- "red"
# map_color[intersect(station_ID_after_May2018, station_ID_before_May2018)] <- "black"
with(records_sample, points(Longitude, Latitude, col = map_color, cex = 1, pch = 20))
# legend("topleft", pch = 20, pt.cex = 1.5, bg = grey(0.8), box.lwd = 0,
#        col = c("red"), legend = c("phased out after May 2018"))

# zoom-in of NY city
par(mfrow = c(1,1), mar = c(1,1,1,1))
xy <- map("county", xlim = c(-74.5, -73.5), ylim = c(40.5, 41), resolution = 0)
with(records_sample, points(Longitude, Latitude, col = map_color, cex = 1.5, pch = 20))
legend("bottomright", pch = 20, pt.cex = 1.5, bg = grey(0.8), box.lwd = 0,
       col = c("red"), legend = c("phased out after May 2018"))

plotly::plot_ly(data = records_sample, x = ~Longitude, y = ~Latitude, mode = 'markers')
                # hoverinfo = 'text', text = ~paste('Station ID: ', Station_ID))


