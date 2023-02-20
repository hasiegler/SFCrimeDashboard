library(shiny)
library(tidyverse)
library(RSocrata)
library(leaflet)
library(DT)

yesterday_df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", Sys.Date() - 1))


yesterday_df <- yesterday_df %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

unique_incident_categories <- yesterday_df %>% 
  count(incident_category, sort = TRUE) %>% 
  pull(incident_category)

labels <- paste("<strong>", "<font size='+0.2'>", yesterday_df$incident_description, "</font>", "</strong>",
                "<br>",
                "<strong>", "Incident Date: ", "</strong>", yesterday_df$incident_date, 
                "<br>", 
                "<strong>", "Incident Time: ", "</strong>", yesterday_df$incident_time,
                "<br>",
                "<strong>", "Incident Category: ", "</strong>", yesterday_df$incident_category,
                "<br>",
                "Click for More Information",
                sep = "")

popups <- paste("<strong>", "<font size='+0.2'>", yesterday_df$incident_description, "</font>", "</strong>",
                "<br>",
                "<strong>", "Incident Date: ", "</strong>", yesterday_df$incident_date, 
                "<br>", 
                "<strong>", "Incident Time: ", "</strong>", yesterday_df$incident_time,
                "<br>",
                "<strong>", "Incident Category: ", "</strong>", yesterday_df$incident_category,
                "<br>",
                "<strong>", "Report Datetime: ", "</strong>", yesterday_df$report_datetime,
                "<br>",
                "<strong>", "Resolution: ", "</strong>", yesterday_df$resolution,
                "<br>",
                "<strong>", "Intersection: ", "</strong>", yesterday_df$intersection,
                "<br>",
                "<strong>", "Neighborhood: ", "</strong>", yesterday_df$analysis_neighborhood,
                "<br>",
                "<strong>", "Incident ID: ", "</strong>", yesterday_df$incident_id,
                sep = "")

# Define UI for application
ui <- bootstrapPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "
      html, body {
        width: 100%;
        height: 100%;
        font-family: Oswald, sans-serif;
        margin: 0;
        padding: 0;
      }
    ")
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
                                              max = Sys.Date() - 1),
                                    
                                    selectInput("select_incident_category",
                                                label = "Choose Incident Category",
                                                choices = c("Larceny Theft",
                                                            "Malicious Mischief",
                                                            "Motor Vehicle Theft",
                                                            "Assault",
                                                            "Other Miscellaneous",
                                                            "Non-Criminal",
                                                            "Burglary",
                                                            "Recovered Vehicle",
                                                            "Drug Offense",
                                                            "Fraud",
                                                            "Warrant",
                                                            "Lost Property",
                                                            "Robbery",
                                                            "Missing Person",
                                                            "Suspicious Occ",
                                                            "Disorderly Conduct",
                                                            "Miscellaneous Investigation",
                                                            "Offences Against The Family And Children",
                                                            "Other",
                                                            "Other Offenses",
                                                            "Weapons Offense",
                                                            "Traffic Violation Arrest",
                                                            "Weapons Carrying Etc",
                                                            "Stolen Property",
                                                            "Traffic Collision",
                                                            "Arson",
                                                            "Vandalism",
                                                            "Courtesy Report",
                                                            "Case Closure",
                                                            "Forgery And Counterfeiting",
                                                            "Fire Report",
                                                            "Embezzlement",
                                                            "Sex Offense",
                                                            "Suicide",
                                                            "Vehicle Impounded",
                                                            "Vehicle Misplaced",
                                                            "Homicide",
                                                            "Prostitution",
                                                            "Drug Violation",
                                                            "Rape",
                                                            "Liquor Laws",
                                                            "Motor Vehicle Theft?",
                                                            "Suspicious",
                                                            "Gambling",
                                                            "Civil Sidewalks",
                                                            "Human Trafficking (A), Commercial Sex Acts",
                                                            "Human Trafficking, Commercial Sex Acts"
                                                            ))
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
  
  selected_category <- reactive({
    noquote(input$select_incident_category)
  })
  
  df <- reactive({
    read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", 
                        selected_date(), 
                        "&incident_category=", 
                        selected_category()))
  })
  
  output$map <- renderLeaflet({
    leaflet(data = yesterday_df) %>% 
      addTiles() %>% 
      addMarkers(~longitude, ~latitude,
                 label = lapply(labels, HTML),
                 popup = lapply(popups, HTML))
  })
  

  observe({
    
    df <- df() %>% 
      mutate(longitude = as.numeric(longitude),
             latitude = as.numeric(latitude))
  
  
  labels <- paste("<strong>", "<font size='+0.2'>", df$incident_description, "</font>", "</strong>",
                  "<br>",
                  "<strong>", "Incident Date: ", "</strong>", df$incident_date, 
                  "<br>", 
                  "<strong>", "Incident Time: ", "</strong>", df$incident_time,
                  "<br>",
                  "<strong>", "Incident Category: ", "</strong>", df$incident_category,
                  "<br>",
                  "Click for More Information",
                  sep = "")
  
  popups <- paste("<strong>", "<font size='+0.2'>", df$incident_description, "</font>", "</strong>",
                  "<br>",
                  "<strong>", "Incident Date: ", "</strong>", df$incident_date, 
                  "<br>", 
                  "<strong>", "Incident Time: ", "</strong>", df$incident_time,
                  "<br>",
                  "<strong>", "Incident Category: ", "</strong>", df$incident_category,
                  "<br>",
                  "<strong>", "Report Datetime: ", "</strong>", df$report_datetime,
                  "<br>",
                  "<strong>", "Resolution: ", "</strong>", df$resolution,
                  "<br>",
                  "<strong>", "Intersection: ", "</strong>", df$intersection,
                  "<br>",
                  "<strong>", "Neighborhood: ", "</strong>", df$analysis_neighborhood,
                  "<br>",
                  "<strong>", "Incident ID: ", "</strong>", df$incident_id,
                  sep = "")

    leafletProxy("map", data = df) %>% 
      addTiles() %>% 
      clearMarkers() %>% 
      addMarkers(~longitude, ~latitude,
                 label = lapply(labels, HTML),
                 popup = lapply(popups, HTML))
    
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
