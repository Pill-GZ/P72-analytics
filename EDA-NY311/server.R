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
    axis(side = 3, pos = 10, at = seq(0, max(agency_table), 500), las = 2)
  })
  
}
