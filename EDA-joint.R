rm(list = ls())
library(dplyr)

#### Load weather and 311 records #############################################################################################

# install.packages('bit64')
library(data.table)
tally_by_date_agency_wide <- data.table::fread("311_records_by_date_agency.csv")
tally_by_date_agency_wide$Date <- as.Date(tally_by_date_agency_wide$Date)

#### traceplots ##########################################################################################
# matplot
par(mar = c(2,5,1,1), las = 1, mfrow = c(1,1))
matplot(x = tally_by_date_agency_wide$Date, y = tally_by_date_agency_wide[,2:7], 
        xlim = range(tally_by_date_agency_wide$Date) - c(0,50), ylim = c(20, 8000),
        type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
legend("topleft", legend = agencyTypes_collapsed[1:5], col = 1:5, lty = 1)

# separate traceplots - first 5 agencies
par(mar = c(2,5,0,1), las = 1, mfrow = c(5,1))
for (agency in agencyTypes_collapsed[1:5]) {
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,agency]), 
       xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  legend("topleft", legend = agency, bty = 'n')
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}

# separate traceplots - next 5 agencies
par(mar = c(2,5,0,1), las = 1, mfrow = c(5,1))
for (agency in agencyTypes_collapsed[6:10]) {
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,agency]), 
       xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  legend("topleft", legend = agency, bty = 'n')
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}
# separate traceplots - the rest combined
par(mar = c(2,5,0,1), las = 1, mfrow = c(1,1))
for (agency in agencyTypes_collapsed[11]) {
  plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,agency]), 
       xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
       type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
  legend("topleft", legend = agency, bty = 'n')
  axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
}

# traceplot - total number of calls
par(mar = c(2,5,0,1), las = 1, mfrow = c(1,1))
plot(x = tally_by_date_agency_wide$Date, y = rowSums(tally_by_date_agency_wide[,-1]), 
     xlim = range(tally_by_date_agency_wide$Date) - c(0,50), # ylim = c(20, 10000),
     type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
legend("topleft", legend = agency, bty = 'n')
axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))


