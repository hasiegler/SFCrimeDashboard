library(shiny)
library(tidyverse)
library(RSocrata)
library(leaflet)
library(DT)
library(leaflet.extras)
library(lubridate)

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




start_date <- as.Date('2018-01-01')
end_date <- Sys.Date() - 2
dates <- seq(start_date, end_date, by = "month")
dates <- sort(as.Date(dates), decreasing = TRUE)
months_num <- format(dates, "%m")
months_words <- format(dates, "%B")
years <- format(dates, "%Y")
month_years <- paste(months_words, years)



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
             
             
             tabPanel("Crime Heatmap",
                      div(class="outer",
                          
                          tags$head(
                            
                            includeCSS("style.css")
                          ),
                          
                          leafletOutput("map2", width = "100%", height = "100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 80, left = 50, right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                      
                                        
                      selectInput("select_month", label = "Select Month",
                                  choices = month_years)
                      
                          )
               )),
             
             tabPanel("Crime Type and Time Analysis",

                      fluidRow(
                        column(12,
                               tags$h1("Select Month for Analysis", class = "text-center",
                                       style = "font-size: 36px;"),
                               tags$h4("Only Using Incidents that were marked as 'Cite or Arrest Adult' at Time of Report",
                                      class = "text-center"),
                               selectInput("select_month2", label = "Select Month",
                                           choices = month_years)
                        ),

                      ),

                      tags$hr(),

                      fluidRow(column(width = 6,
                                      strong("Select A NeighborHood In San Francisco"),

                                      ),

                               column(width = 6,
                                      strong("San Francisco as a Whole"),
                                      plotlyOutput("incident_counts")
                                      )
                               ),

                      fluidRow(
                        column(width = 6,
                               strong("test")),

                        column(width = 6,
                               plotlyOutput("hour_day")
                        )
                      )
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
  
  
  
  
  
  
  #############
  
  selected_month_data <- reactive({
    
  month_year_selected <- str_split(input$select_month, " ")
  month_name_selected <- month_year_selected[[1]][1]
  year_selected <- month_year_selected[[1]][2]
  date_selected <- as.Date(str_replace_all(paste(year_selected, "-", month_name_selected, "-", "01"), " ",""), format = "%Y-%B-%d")
  
  read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between ",
                      "'",
                      date_selected, 
                      "' ",
                      "and ",
                      "'",
                      date_selected + 33,
                      "'"
                      ))
  
  })
  

  output$map2 <- renderLeaflet({
    
    today_mon <- format(Sys.Date() - 2, "%m")
    today_year <- as.character(year(Sys.Date() - 2))
    
    today_ymd <- paste0(today_year, "-", today_mon, "-", "01")
    
    df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between ",
                        "'",
                        today_ymd, 
                        "' ",
                        "and ",
                        "'",
                        as.Date(today_ymd) + 33,
                        "'"))
    
    df <- df %>% 
      mutate(longitude = as.numeric(longitude),
             latitude = as.numeric(latitude)) %>% 
      filter(!is.na(longitude) & !is.na(latitude)) 
    
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 50px; 
    padding-right: 50px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 1000px;
  }
"))
    
    title <- tags$div(
      tag.map.title, HTML(paste(month_years[1], "Crime Heatmap"))
    ) 
    
    leaflet(data = df) %>% 
      addTiles() %>% 
      addHeatmap(lng= ~longitude, lat = ~latitude, max = 5, radius = 40, blur = 25) %>% 
      addControl(title, position = "topright")
    
  })
  
  observe({
    df <- selected_month_data()
    
    df <- df %>% 
      mutate(longitude = as.numeric(longitude),
             latitude = as.numeric(latitude)) %>% 
      filter(!is.na(longitude) & !is.na(latitude)) 
    
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 50px; 
    padding-right: 50px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 1000px;
  }
"))
    
    title <- tags$div(
      tag.map.title, HTML(paste(input$select_month, "Crime Heatmap"))
    ) 
    
    leafletProxy("map2", data = df) %>%
      addTiles() %>%
      clearHeatmap() %>% 
      addHeatmap(lng= ~longitude, lat = ~latitude, max = 5, radius = 40, blur = 25) %>% 
      clearControls() %>% 
      addControl(title, position = "topright")
    
  })
  
  
  
  #################
  
  selected_month_data2 <- reactive({
    
    month_year_selected <- str_split(input$select_month2, " ")
    month_name_selected <- month_year_selected[[1]][1]
    year_selected <- month_year_selected[[1]][2]
    date_selected <- as.Date(str_replace_all(paste(year_selected, "-", month_name_selected, "-", "01"), " ",""), format = "%Y-%B-%d")
    
    read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between ",
                        "'",
                        date_selected, 
                        "' ",
                        "and ",
                        "'",
                        date_selected + 33,
                        "'"
    ))
    
  })
  
  output$incident_counts <- renderPlotly({
    
    df <- selected_month_data2()
    
    cats <- df %>% 
      count(incident_category, sort = TRUE)
    
    p <- ggplot(cats, aes(x = reorder(incident_category, n),
                     y = n)) +
      geom_segment(aes(xend = incident_category,
                       yend = 0),
                   color = "gray") + 
      geom_point(aes(text = paste(incident_category, "Count: ", n)),
                 color = "red",
                 size = 2) + 
      coord_flip() + 
      theme_minimal() + 
      xlab("") + 
      ylab("Number of Incidents") + 
      theme(axis.text.y = element_text(size = 6))
    
    ggplotly(p, tooltip = "text")
  
  })
  
  output$hour_day <- renderPlotly({
    
    df <- selected_month_data2()
    
    df_most_common_incident <- df %>% 
      mutate(incident_hour = hour(strptime(incident_time, format = "%H:%M"))) %>% 
      group_by(incident_hour) %>% 
      count(incident_category, sort = TRUE) %>% 
      group_by(incident_hour) %>% 
      slice(which.max(n))
    
    df_count_hour <- df %>% 
      mutate(incident_hour = hour(strptime(incident_time, format = "%H:%M"))) %>% 
      count(incident_hour) %>% 
      rename(number = n)
    
    full_df <- full_join(df_count_hour, df_most_common_incident, by = "incident_hour")
    
    full_df <- full_df %>% 
      mutate(incident_hour_format = strftime(strptime(sprintf("%02d", incident_hour), format = "%H"), format = "%I %p"),
             incident_hour_format = gsub("^0", "", incident_hour_format))
    
    p <- full_df %>% 
      ggplot(aes(x = as.factor(incident_hour),
                 y = number)) + 
      geom_col(aes(text = paste("Most Common Incident Category:", 
                                incident_category,
                                "\nCount:",
                                n))) +
      theme_minimal() + 
      xlab("Hour of the Day") + 
      ylab("Number of Incidents") + 
      scale_x_discrete(labels = full_df$incident_hour_format)
    
    ggplotly(p, tooltip = "text")
    
  })
  
  

}


# Run the application 
shinyApp(ui = ui, server = server)
