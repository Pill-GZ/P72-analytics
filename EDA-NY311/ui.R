library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("311 what's your non-emergency?", id="nav", # theme = "bootstrap-cosmo-customized.css",
           tabPanel("Interactive map", 
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        
                        leafletOutput("mymap",height = 1000),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
    
                                      h2("Data filter"),
    
                                      dateInput('date',
                                                label = paste('Select date'),
                                                value = as.character(as.Date("2015-04-17")),
                                                min = as.Date("2010-01-01"), max = as.Date("2019-08-07"),
                                                format = "dd/mm/yy",
                                                startview = 'year', weekstart = 0
                                      ),
                                      
                                      numericInput("max_num_events", "Max number of events to display", 100),
                                      
                                      selectInput("agency", "Filter by agency", c("All", agencyTypes, "Others")),
                                      
                                      plotOutput("histCentile", height = 400)
                        )
                    )
                
           )
)

  
# 
# navbarPage("Superzip", id="nav",
# 
#            tabPanel("Interactive map",
#                     div(class="outer",
# 
#                         tags$head(
#                           # Include our custom CSS
#                           includeCSS("styles.css"),
#                           includeScript("gomap.js")
#                         ),
# 
#                         # If not using custom CSS, set height of leafletOutput to a number instead of percent
#                         leafletOutput("mymap", width="100%", height="100%"),
# 
#                         # Shiny versions prior to 0.11 should use class = "modal" instead.
#                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#                                       width = 330, height = "auto",
# 
#                                       h2("ZIP explorer"),
#                                       #
#                                       # selectInput("color", "Color", vars),
#                                       # selectInput("size", "Size", vars, selected = "adultpop"),
#                                       # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
#                                       #                  # Only prompt for threshold when coloring or sizing by superzip
#                                       #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
#                                       # ),
#                                       #
#                                       # plotOutput("histCentile", height = 200),
#                                       # plotOutput("scatterCollegeIncome", height = 250)
#                         )
#                     )
#            )
# 
#            # tabPanel("Data explorer"
#            # ),
# 
# )