library(shiny)
library(tidyverse)
library(RSocrata)
library(leaflet)
library(DT)

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
                      strong("Testing"),
                      dataTableOutput("new_data")
               
             )
  )
  

  
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  selected_date <- reactive({
    as.character(input$date)
  })
  
  yesterday_df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", Sys.Date() - 1))
  
  df <- reactive({
    read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", selected_date()))
  })
  
  yesterday_df <- yesterday_df %>% 
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude))
  
  
  labels <- paste("<strong>", "<font size='+0.2'>", yesterday_df$incident_description, "</font>", "</strong>",
                  "<br>",
                  "<strong>", "Date: ", "</strong>", yesterday_df$incident_date, 
                  "<br>", 
                  "<strong>", "Time: ", "</strong>", yesterday_df$incident_time,
                  "<br>",
                  "<strong>", "Incident Category: ", "</strong>", yesterday_df$incident_category,
                  sep = "")
  
  output$map <- renderLeaflet({
    leaflet(data = yesterday_df) %>% 
      addTiles() %>% 
      addMarkers(~longitude, ~latitude,
                 label = lapply(labels, HTML))
  })
  
  observe({
    
  df <- df() %>% 
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
  

    leafletProxy("map", data = df) %>% 
      addTiles() %>% 
      clearMarkers() %>% 
      addMarkers(~longitude, ~latitude,
                 label = lapply(labels, HTML))
    
  })
 
  
  observe({
    new_data <- df()
    
    output$new_data <- renderDataTable({
      datatable(new_data)
    })
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
