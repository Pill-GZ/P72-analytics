
library(plotly)
library(shiny)
library(dplyr)

df <- data.table::fread("../311_records_sample_n10000.csv")
df <- df %>% 
  mutate(Date = lubridate::mdy(substr(Created.Date, start = 1, stop = 10)))

# 1 HPD 2 NYPD 3 DOT 4 DSNY 5 DEP 6 DOB 7 DPR 8 DOHMH 9 DOF 10 TLC
eventColors <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', 
                 '#ffff33', '#a65628', '#f781bf', '#999999')

agencyTypes <- c('HPD', 'NYPD', 'DOT', 'DSNY', 'DEP', 'DOB', 'DPR', 'DOHMH')