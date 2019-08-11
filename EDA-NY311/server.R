library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

function(input,output, session){
  
  data <- reactive({
    # ramdomly select events
    x <- df %>% sample_n(input$max_num_events) 
    # filter by dates
    # x <- x %>% filter(Date == input$date)
    # filter by agency types
    if (input$agency %in% agencyTypes) {
      x <- x %>% filter(Agency == input$agency)
    } else if (input$agency == "Others") {
      x <- x %>% filter(!Agency %in% agencyTypes)
    }
    x
  })
  
  icons <- reactive({
    awesomeIcons(
      icon = c('flag'),
      library = 'ion',
      markerColor = 'white',
      iconColor = eventColors[match(x = data()$Agency, nomatch = 9, 
                                    table = agencyTypes)]
    )
  })
  
  output$mymap <- renderLeaflet({
    filtered.df <- data()
    
    m <- leaflet(data = filtered.df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # addTiles() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, icon = icons(), 
                        popup = paste("Type: ", filtered.df$Complaint.Type, "<br>",
                                      "Agency: ", filtered.df$Agency, "<br>",
                                      "Date: ", filtered.df$Created.Date))
    m
  })
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(data()) == 0)
      return(NULL)
    
    tbl <- df %>% group_by(Agency) %>% summarise(count = n_distinct(Unique.Key)) %>% 
      arrange(match(Agency, agencyTypes), desc(count)) # arrange(desc(count))
    par(mar = c(0,5,3,1))
    barplot(rev(tbl$count[1:length(agencyTypes)]), names.arg = rev(agencyTypes),
            las = 2, horiz = T, xaxt = 'n')
    axis(side = 3, pos = 10, at = seq(0, max(tbl$count), 500), las = 2)
  })
  
  
  # set quantiles of weather variables
  weather_var_val <- reactive({
    weather_NYC_by_var_wide %>% 
      pluck(input$weather_var_name, 'Average')
  })
  
  threshold <- reactive({
    quantile(weather_var_val(), probs = input$percentile, na.rm = T)
  })
  
  output$traceplot <- renderPlot({
    # shaded traceplots - first 5 agencies
    par(mar = c(2,5,0,1), las = 1, mfrow = c(11,1))
    for (agency in agencyTypes_collapsed) {
      # create empty plot
      plot(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), 
           xlim = range(Dates) - c(0,50), width = 800, 
           type = 'l', lty = 1, xaxs = 'i', xaxt = 'n', ylab = "")
      
      # shade weather variables
      threshold_exceedance <- if (input$shade_val_above) { 
        weather_var_val() > threshold()
      } else { weather_var_val() < threshold() } 
      threshold_exceedance[is.na(threshold_exceedance)] <- 0
      polygon(x = c(rep(c(Dates[1], Dates), each = 2)), 
              y = c(0, rep(threshold_exceedance, each = 2), 0)* 10000, col = "grey80", border = NA)
      
      # re-draw the 311 records
      lines(x = tally_by_date_agency_wide$Date, y = unlist(tally_by_date_agency_wide[,..agency]), lty = 1)
      
      # legend and axis
      legend("topleft", legend = agency, bty = 'n', cex = 3)
      legend("topright", fill = "grey80", bty = 'n', cex = 3,
             legend = paste(input$weather_var_name, 
                            ifelse(input$shade_val_above, ">", "<"), 
                            format(round(threshold(), 2), nsmall = 2)))
      axis(side = 1, at = lubridate::ymd(paste0(seq(2010, 2020, 2), "0101")), labels = seq(2010, 2020, 2))
    }
  })
  
  
}
