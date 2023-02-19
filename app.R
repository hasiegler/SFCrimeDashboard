library(shiny)
library(tidyverse)
library(RSocrata)
library(leaflet)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}")
    ),
  
  navbarPage("San Francisco Crimes", id="nav",
             
             tabPanel("Interactive Map",
                      
                      leafletOutput("map"),
                      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Choose Date and Category"),
                                    
                                    dateInput("date",
                                              label = "Choose Date",
                                              value = Sys.Date() - 1,
                                              min = "2018-01-01",
                                              max = Sys.Date() - 1)
                      )
                          
              ),
             
             
             tabPanel("Data Exploration",
                      strong("Testing")
               
             )
  )
  

  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- read.socrata(
    "https://data.sfgov.org/resource/wg3w-h783.json?incident_date=2023-02-11"
  )
  
  df <- df %>% 
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude))
  
  
  labels <- paste("<strong>", "<font size='+0.2'>", df$incident_description, "</font>", "</strong>",
                  "<br>",
                  "<strong>", "Date: ", "</strong>", df$incident_date, 
                  "<br>", 
                  "<strong>", "Time: ", "</strong>", df$incident_time,
                  "<br>",
                  "<strong>", "Incident Category: ", "</strong>", df$incident_category,
                  sep = "")
  

    output$map <- renderLeaflet({
      leaflet(data = df) %>% 
        addTiles() %>% 
        addMarkers(~longitude, ~latitude,
                   label = lapply(labels, HTML))
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
