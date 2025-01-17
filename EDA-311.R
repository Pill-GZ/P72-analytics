rm(list = ls())
library(dplyr)

#### Load weather dataset #############################################################################################

# install.packages('bit64')
library(data.table)
records <- data.table::fread("311_Service_Requests_from_2010_to_Present.csv")

#### data wrangling ####################################################################################################

# clean up column names
colnames(records) <- make.names(colnames(records), unique = TRUE)

# check for missing dates -- no missing dates
anyNA(records$Created.Date)

# create dates
records <- records %>% 
  mutate(Date = lubridate::mdy(substr(Created.Date, start = 1, stop = 10)))

#### create sample records #############################################################################################

# blocks of dates -- Jan 12-18, 2015
records_sample_Jan2015 <- records %>% 
  filter(Date >= lubridate::ymd('20150112'), 
         Date <= lubridate::ymd('20150118'))
fwrite(x = records_sample_Jan2015, file = "311_records_Jan2015.csv")

# blocks of dates -- Jul 20-26, 2015
records_sample_Jul2015 <- records %>% 
  filter(Date >= lubridate::ymd('20150720'), 
         Date <= lubridate::ymd('20150726'))
fwrite(x = records_sample_Jul2015, file = "311_records_Jul2015.csv")

# random samples of 10,000
records_sample <- sample_n(records, size = 10000)
fwrite(x = records_sample, file = "311_records_sample_n10000.csv")
# records_sample <- sample_n(records, size = 100000)
# fwrite(x = records_sample, file = "311_records_sample_n100000.csv")

#### create counts by categories -- Agency ##################################################################################

# explore distribution of agency types

agency_table <- table(factor(records$Agency))
agency_table <- sort(agency_table, decreasing = T)
length(agency_table)

par(mar = c(5,3,1,1), mfrow = c(2,1))
barplot(agency_table / sum(agency_table), las = 2, ylim = c(0, 0.3))
text(20, 0.2, labels = "Most of the calls are handled by a few deptments ...")

barplot(cumsum(agency_table) / sum(agency_table), ylim = c(0,1), las = 2)
mtext(text = "... top 10 departments handles 97% of the calls.", side = 3, at = 0, adj = 0)
abline(h = 0.95, lty = 2)

# tally number of calls by agency
tally_by_agency <- records %>% 
  group_by(Agency, Agency.Name) %>% 
  summarise(n_records = n_distinct(Unique.Key)) %>%
  arrange(desc(n_records))

tally_by_agency <- tally_by_agency %>% 
  group_by(Agency) %>%
  mutate(total_records_by_agency = sum(n_records)) %>%
  arrange(desc(total_records_by_agency))

# collapse agency types
agencyTypes_all <- unlist(tally_by_agency %>% distinct(Agency))
# Top 10 hadling agencies
# HPD  # NYPD # DOT  # DSNY # DEP  # DOB  # DPR  # DOHMH # DOF # TLC
agencyTypes_collapsed <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 
                           'DOB', 'DPR', 'DOHMH', 'DOF', 'TLC',
                           rep('Others', length(agencyTypes_all) - 10))
cbind(agencyTypes_all, agencyTypes_collapsed)

# records_copy <- records # save a copy first

# create column for collapsed agency types
records <- records %>%
  mutate(Agency.grouped = plyr::mapvalues(Agency, from = agencyTypes_all, to = agencyTypes_collapsed))

# tally by date and (collapsed) agency types
tally_by_date_agency <- records %>%
  group_by(Date, Agency.grouped) %>%
  tally(sort = TRUE) %>% arrange(Date)

tally_by_date_agency

# convert long to wide format
library(tidyr)
tally_by_date_agency_wide <- tally_by_date_agency %>%
  spread(Agency.grouped, n) %>%
  replace(., is.na(.), 0) %>% 
  select(Date, agencyTypes_collapsed)

fwrite(x = tally_by_date_agency_wide, file = "311_records_by_date_agency.csv")


#### complaint type ####################################################################

complaint_type_summary <- summary(factor(records$Complaint.Type))
plot(complaint_type_summary)
sort(names(complaint_type_summary) )

complaint_type_table <- table(factor(records$Complaint.Type))
complaint_type_table <- sort(complaint_type_table, decreasing = T)
length(complaint_type_table)

par(mar = c(15,3,1,1), mfrow = c(1,1))
plot(complaint_type_table, las = 3)
par(mar = c(3,3,1,1), mfrow = c(1,1))
plot(cumsum(complaint_type_table) / sum(complaint_type_table), ylim = c(0,1), type = 'l')
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

#### agency 
#### locations ###################################################################################

location_table <- table(factor(records$Park.Borough))
location_table <- sort(location_table, decreasing = T)
length(location_table)

par(mar = c(8,5,1,1))
barplot(location_table / sum(location_table), las = 2, ylim = c(0, 0.3))


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


