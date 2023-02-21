library(shiny)
library(tidyverse)
library(RSocrata)
library(leaflet)
library(DT)

yesterday_df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", Sys.Date() - 2))


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

yesterday_incident_categories <- yesterday_df %>%
  count(incident_category, sort = TRUE) %>%
  pull(incident_category)



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
                      div(class="outer",
                          
                          tags$head(
                            
                            includeCSS("style.css")
                          ),
                
                      leafletOutput("map", width = "100%", height = "100%"),
                      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 80, left = 50, right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Filter Incidents by Date and Incident Category"),
                                    
                                    dateInput("date",
                                              label = "Choose Date",
                                              value = Sys.Date() - 2,
                                              min = "2018-01-01",
                                              max = Sys.Date() - 2),
                                    
                                    conditionalPanel(condition = "input.all_incidents == 0",
                                    selectInput("incident_type",
                                                label = "Choose Incident Category",
                                                choices = c("Larceny Theft",
                                                            "Malicious Mischief",
                                                            "Assault",
                                                            "Motor Vehicle Theft",
                                                            "Other Miscellaneous",
                                                            "Non-Criminal",
                                                            "Burglary",
                                                            "Recovered Vehicle",
                                                            "Fraud",
                                                            "Drug Offense",
                                                            "Lost Property",
                                                            "Warrant",
                                                            "Robbery",
                                                            "Suspicious Occ",
                                                            "Missing Person",
                                                            "Disorderly Conduct",
                                                            "Offences Against The Family And Children",
                                                            "Miscellaneous Investigation",
                                                            "Other",
                                                            "Other Offenses",
                                                            "Weapons Offense",
                                                            "Weapons Carrying Etc",
                                                            "Traffic Violation Arrest",
                                                            "Stolen Property",
                                                            "Courtesy Report",
                                                            "Arson",
                                                            "Vandalism",
                                                            "Traffic Collision",
                                                            "Case Closure",
                                                            "Forgery And Counterfeiting",
                                                            "Fire Report",
                                                            "Embezzlement",
                                                            "Sex Offense",
                                                            "Suicide",
                                                            "Vehicle Impounded",
                                                            "Drug Violation",
                                                            "Vehicle Misplaced",
                                                            "Homicide",
                                                            "Liquor Laws",
                                                            "Prostitution",
                                                            "Rape",
                                                            "Suspicious",
                                                            "Gambling",
                                                            "Human Trafficking (A), Commercial Sex Acts",
                                                            "Motor Vehicle Theft?",
                                                            "Civil Sidewalks",
                                                            "Human Trafficking, Commercial Sex Acts"))
                                    ),
                                    
                                    checkboxInput("all_incidents", label = "Check Box to See All Incidents of Selected Date",
                                                  value = TRUE)
                      )
                      )
                          
              ),
             
             
             tabPanel("Data Exploration",
                      strong("Testing"),
                      plotOutput("testplot")
               
             )
  )
  

  
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  selected_date <- reactive({
    as.character(input$date)
  })
  

  
  df_day <- reactive({
     read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", 
                        selected_date()))
  })
  

  output$map <- renderLeaflet({
    leaflet(data = yesterday_df) %>% 
      addTiles() %>% 
      addMarkers(~longitude, ~latitude,
                 label = lapply(labels, HTML),
                 popup = lapply(popups, HTML))
  })
  
  
  df_day_incident <- reactive({

    if(input$all_incidents){
      df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=",
                            input$date))
    } else{
      df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=",
                                input$date))
      
      df <- df %>% 
        filter(incident_category == input$incident_type)
    }
    
    return(df)
  })
  

  observe({

    df <- df_day_incident()

    df <- df %>%
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


  output$testplot <- renderPlot({
    
    df <- df_day() %>% 
      filter(incident_category == input$incident_type)
    
    
    
    df %>% 
      ggplot(aes(x = analysis_neighborhood)) + 
      geom_bar() + 
      coord_flip()
      
  })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
