library(shiny)          # load the Shiny package for building web apps
library(tidyverse)      # load the Tidyverse package for data wrangling and visualization
library(RSocrata)       # load the RSocrata package for working with Socrata Open Data API
library(leaflet)        # load the Leaflet package for interactive maps
library(DT)             # load the DT package for interactive tables
library(leaflet.extras) # load the Leaflet.Extras package for additional functionality
library(lubridate)      # load the lubridate package for working with dates and times
library(plotly)         # load the Plotly package for interactive plots


# read the Socrata Open Data API endpoint for San Francisco incidents that occurred yesterday
# using the RSocrata package and store it in yesterday_df
yesterday_df <- read.socrata(paste0("https://data.sfgov.org/resource/wg3w-h783.json?incident_date=", 
                                    Sys.Date() - 2))

# convert the longitude and latitude columns from characters to numerics
yesterday_df <- yesterday_df %>%
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude))

# store the unique incident categories in unique_incident_categories
unique_incident_categories <- yesterday_df %>% 
  count(incident_category, sort = TRUE) %>% 
  pull(incident_category)

# create labels for the markers on the map, which include the incident description, date, time, and category
labels <- paste("<strong>", "<font size='+0.2'>", yesterday_df$incident_description, "</font>", "</strong>", "<br>",
                "<strong>", "Incident Date: ", "</strong>", yesterday_df$incident_date, "<br>",
                "<strong>", "Incident Time: ", "</strong>", yesterday_df$incident_time, "<br>",
                "<strong>", "Incident Category: ", "</strong>", yesterday_df$incident_category, "<br>",
                "Click for More Information", sep = "")

# create popups for the markers on the map, which include additional information about the incident
popups <- paste("<strong>", "<font size='+0.2'>", yesterday_df$incident_description, "</font>", "</strong>", "<br>",
                "<strong>", "Incident Date: ", "</strong>", yesterday_df$incident_date, "<br>",
                "<strong>", "Incident Time: ", "</strong>", yesterday_df$incident_time, "<br>",
                "<strong>", "Incident Category: ", "</strong>", yesterday_df$incident_category, "<br>",
                "<strong>", "Report Datetime: ", "</strong>", yesterday_df$report_datetime, "<br>",
                "<strong>", "Resolution: ", "</strong>", yesterday_df$resolution, "<br>",
                "<strong>", "Intersection: ", "</strong>", yesterday_df$intersection, "<br>",
                "<strong>", "Neighborhood: ", "</strong>", yesterday_df$analysis_neighborhood, "<br>",
                "<strong>", "Incident ID: ", "</strong>", yesterday_df$incident_id, sep = "")

# store the unique incident categories in yesterday_incident_categories
yesterday_incident_categories <- yesterday_df %>% 
  count(incident_category, sort = TRUE) %>% 
  pull(incident_category)

# create a vector of all of the incident categories that have occurred in SF
all_incident_categories <- c(
  "Assault",
  "Malicious Mischief",
  "Larceny Theft",
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
  "Human Trafficking, Commercial Sex Acts"
)

# create a vector of all neighborhoods in SF
all_neighborhoods <- c(
  "Tenderloin",
  "South of Market",
  "Mission",
  "Financial District/South Beach",
  "Bayview Hunters Point",
  "Western Addition",
  "Nob Hill",
  "Castro/Upper Market",
  "Marina",
  "North Beach",
  "Mission Bay",
  "Outer Richmond",
  "Sunset/Parkside",
  "Bernal Heights",
  "Hayes Valley",
  "Outer Mission",
  "West of Twin Peaks",
  "Chinatown",
  "Potrero Hill",
  "Pacific Heights",
  "Russian Hill",
  "Excelsior",
  "Visitacion Valley",
  "Lone Mountain/USF",
  "Portola",
  "Oceanview/Merced/Ingleside",
  "Haight Ashbury",
  "Inner Richmond",
  "Lakeshore",
  "Noe Valley",
  "Inner Sunset",
  "Golden Gate Park",
  "Japantown",
  "Glen Park",
  "Presidio Heights",
  "Twin Peaks",
  "Treasure Island",
  "McLaren Park",
  "Seacliff",
  "Presidio",
  "Lincoln Park"
)

# earliest date in the SF database
start_date <- as.Date('2018-01-01')

# date with latest crime data
end_date <- Sys.Date() - 2

# create a vector of dates by month going from start date to end date
dates <- seq(start_date, end_date, by = "month")

# order the months in decreasing order
dates <- sort(as.Date(dates), decreasing = TRUE)

# create a vector of the number of the month for each date
months_num <- format(dates, "%m")

# create a vector of the name of the month for each date
months_words <- format(dates, "%B")

# create a vector of the year of the month for each date
years <- format(dates, "%Y")

# create a vecotr of the month in words and year for each date
month_years <- paste(months_words, years)



##################################



# Define UI for application
ui <- bootstrapPage(
  
  # add a theme 
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  
  # create a navigation bar at the top of the website
  navbarPage(
    "San Francisco Crimes",
    id = "nav",
    
    tabPanel(
      "Interactive Map",
      div(
        class = "outer",
        
        tags$head(includeCSS("style.css")),
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 80,
          left = 50,
          right = 20,
          bottom = "auto",
          width = 330,
          height = "auto",
          
          h2("Filter Incidents by Date and Incident Category"),
          
          dateInput(
            "date",
            label = "Choose Date",
            value = Sys.Date() - 2,
            min = "2018-01-01",
            max = Sys.Date() - 2
          ),
          
          conditionalPanel(
            condition = "input.all_incidents == 0",
            selectInput("incident_type",
                        label = "Choose Incident Category",
                        choices = all_incident_categories)
          ),
          
          checkboxInput("all_incidents", label = "Check Box to See All Incidents of Selected Date",
                        value = TRUE)
        )
      )
      
    ),
    
    
    tabPanel(
      "Crime Heatmap",
      div(
        class = "outer",
        
        tags$head(includeCSS("style.css")),
        
        leafletOutput("map2", width = "100%", height = "100%"),
        
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 80,
          left = 50,
          right = 20,
          bottom = "auto",
          width = 330,
          height = "auto",
          
          selectInput("select_month", 
                      label = "Select Month",
                      choices = month_years),
          
          h6("Only Using Incidents that were marked as 'Cite or Arrest Adult' at Time of Report")
        )
      )
    ),
    
    tabPanel(
      "Crime Analysis",
      
      fluidRow(
        column(
          12,
          tags$h1("Select Month for Analysis", class = "text-center",
                  style = "font-size: 36px;"),
          tags$h6(
            "Only Using Incidents that were marked as 'Cite or Arrest Adult' at Time of Report",
            class = "text-center"
          ),
          div(
            class = "column",
            
            tags$head(includeCSS("style.css")),
            selectInput("select_month2",
                        label = NULL,
                        choices = month_years)
          )
        ),
        
        tags$hr(),
        
        fluidRow(
          column(
            width = 6,
            tags$h4("Single NeighborHood In San Francisco",
                    class = "text-center"),
            tags$h6("Select a Neighborhood",
                    class = "text-center"),
            
            selectInput(
              inputId = "select_neighborhood",
              label = NULL,
              choices = all_neighborhoods
            )
          ),
          
          column(width = 6,
                 tags$h4("San Francisco as a Whole",
                         class = "text-center"))
        ),
        
        tags$hr(),
        
        fluidRow(
          column(
            width = 6,
            plotlyOutput("incident_counts_neighborhood", height = "600px")
          ),
          
          column(width = 6,
                 plotlyOutput("incident_counts", height = "600px"))
        ),
        
        tags$br(),
        
        tags$br(),
        
        tags$br(),
        
        tags$hr(),
        
        tags$br(),
        
        tags$br(),
        
        tags$br(),
        
        fluidRow(
          column(
            width = 6,
            plotlyOutput("hour_day_neighborhood", height = "500px")
          ),
          
          column(width = 6,
                 plotlyOutput("hour_day", height = "500px"))
        ),
        
        tags$br(),
        
        tags$br(),
        
        tags$br(),
        
        tags$hr(),
        
        tags$br(),
        
        tags$br(),
        
        tags$br(),
        
        fluidRow(plotlyOutput("neighborhood_counts", height = "600px")),
        
        tags$br(),
        
        tags$br(),
        
        tags$br()
      )
    ),
    
    tabPanel(
      "Crime Data Table",
      
      fluidRow(column(
        12,
        tags$h1(
          "Search for Individual Crimes",
          class = "text-center",
          style = "font-size: 36px;"
        ),
        tags$h6(
          "Only Using Incidents that were marked as 'Cite or Arrest Adult' at Time of Report",
          class = "text-center"
        ),
        div(
          class = "column",
          
          tags$head(includeCSS("style.css")),
          tags$h5("Filter by Month",
                  class = "text-center"),
          
          selectInput("select_month_search",
                      label = NULL,
                      choices = month_years)
        )
      )),
      
      fluidRow(column(
        12,
        div(
          class = "column",
          
          tags$head(includeCSS("style.css")),
          tags$h5("Filter by Neighborhood",
                  class = "text-center"),
          
          selectInput(
            "select_neighborhood_search",
            label = NULL,
            choices = all_neighborhoods
          )
          
          
        )
      )),
      
      tags$hr(),
      
      column(12,
             dataTableOutput("table_search"))
    ),
    
    tabPanel("About",

             fluidRow(column(
               width = 12,
               
               tags$h1("About this App",
                       class = "text-center"),
               
               tags$h2("Contact"),
               
               "Henry Siegler", 
               
               tags$br(),
               
               "hsiegler@calpoly.edu", 
               
               tags$br(),
               
               tags$a("LinkedIn", href = "https://www.linkedin.com/in/henrysiegler/"), 
               
               tags$br(),
               
               tags$a("GitHub", href = "https://hasiegler.github.io/Portfolio/"),
               
               tags$h2("Source"),
               
               tags$a("DataSF: Police Department Incident Reports: 2018 to Present",
                      href = "https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-2018-to-Present/wg3w-h783"),
               
               tags$h2("Code"),
               
               "This web application is written using the R Shiny web framework.", 
               
               tags$br(),
               
               "Code used to generate this Shiny app are available on my", 
               
               tags$a("GitHub", href = "https://github.com/hasiegler/SFCrimeDashboard"),
               
               tags$h2("App"),
               
               "The purpose of this application is to provide an interactive and comprehensive visualization of crime data in the city of San Francisco. 
               Its primary aim is to enable users to see where crimes are occuring and gain insights into the types of crimes that occur in various areas of the city, and the times when such 
               crimes are more likely to take place. Through the use of this application, users can acquire an informed understanding of the patterns and trends in 
               criminal activity in San Francisco, and stay updated on the latest information regarding reported crimes in the city. 
               The application employs various visualization techniques to represent the data in an easy-to-understand format, 
               facilitating users to explore the data and comprehend the crime situation in the city with greater clarity.",
               
               tags$br(),
               tags$br(),
               
               "To fetch the daily crime data, the application uses an API offered by the San Francisco Police Department. 
               Crime reports are submitted by officers or the general public using SFPD’s online reporting system, with data being 
               added to the database after the incident reports have been reviewed and approved by a supervising Sergeant or Lieutenant.",
               
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br()

             )))
  )
)







# Define server for app
server <- function(input, output) {
  
  # INTERACTIVE MAP SECTION
  
  # create a function that makes an API call for the selected date
  df_day <- reactive({
    read.socrata(
      paste0(
        "https://data.sfgov.org/resource/wg3w-h783.json?incident_date=",
        input$date
      )
    )
  })
  
  
  # create leaflet map for yesterday's data
  output$map <- renderLeaflet({
    leaflet(data = yesterday_df) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addMarkers(
        ~ longitude,
        ~ latitude,
        label = lapply(labels, HTML),
        popup = lapply(popups, HTML)
      )
  })
  
  # create a function that makes an API call for the selected date and incident category, if selected
  df_day_incident <- reactive({
    if (input$all_incidents) {
      df <-
        read.socrata(
          paste0(
            "https://data.sfgov.org/resource/wg3w-h783.json?incident_date=",
            input$date
          )
        )
    } else{
      df <-
        read.socrata(
          paste0(
            "https://data.sfgov.org/resource/wg3w-h783.json?incident_date=",
            input$date
          )
        )
      
      df <- df %>%
        filter(incident_category == input$incident_type)
    }
    return(df)
  })
  
  
  # update the leaflet map to only show incidents for selected date and/or incident category
  observe({
    df <- df_day_incident()
    
    df <- df %>%
      mutate(longitude = as.numeric(longitude),
             latitude = as.numeric(latitude))
    
    labels <- paste("<strong>", "<font size='+0.2'>", df$incident_description, "</font>", "</strong>", "<br>",
                    "<strong>", "Incident Date: ", "</strong>", df$incident_date, "<br>",
                    "<strong>", "Incident Time: ", "</strong>", df$incident_time, "<br>",
                    "<strong>", "Incident Category: ", "</strong>", df$incident_category, "<br>",
                    "Click for More Information", sep = "")
    
    # create popups for the markers on the map, which include additional information about the incident
    popups <- paste("<strong>", "<font size='+0.2'>", df$incident_description, "</font>", "</strong>", "<br>",
                    "<strong>", "Incident Date: ", "</strong>", df$incident_date, "<br>",
                    "<strong>", "Incident Time: ", "</strong>", df$incident_time, "<br>",
                    "<strong>", "Incident Category: ", "</strong>", df$incident_category, "<br>",
                    "<strong>", "Report Datetime: ", "</strong>", df$report_datetime, "<br>",
                    "<strong>", "Resolution: ", "</strong>", df$resolution, "<br>",
                    "<strong>", "Intersection: ", "</strong>", df$intersection, "<br>",
                    "<strong>", "Neighborhood: ", "</strong>", df$analysis_neighborhood, "<br>",
                    "<strong>", "Incident ID: ", "</strong>", df$incident_id, sep = "")
    
    leafletProxy("map", data = df) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      clearMarkers() %>%
      addMarkers(
        ~ longitude,
        ~ latitude,
        label = lapply(labels, HTML),
        popup = lapply(popups, HTML)
      )
  })
  
  
  
  # CRIME HEATMAP SECTION
  
  
  # create a function that makes an API call for the selected month
  selected_month_data <- reactive({
    
    month_year_selected <- str_split(input$select_month, " ")
    
    month_name_selected <- month_year_selected[[1]][1]
    
    year_selected <- month_year_selected[[1]][2]
    
    date_selected <-
      as.Date(str_replace_all(
        paste(year_selected, "-", month_name_selected, "-", "01"),
        " ",
        ""
      ), format = "%Y-%B-%d")
    
    last_day_month <- ceiling_date(date_selected, unit = "month") - days(1)
    
    read.socrata(
      paste0(
        "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between ",
        "'",
        date_selected,
        "' ",
        "and ",
        "'",
        last_day_month,
        "'"
      )
    )
    
  })
  
  
  #create leaflet heatmap for current month
  output$map2 <- renderLeaflet({
    
    today_mon <- format(Sys.Date() - 2, "%m")
    
    today_year <- as.character(year(Sys.Date() - 2))
    
    today_ymd <- as.Date(paste0(today_year, "-", today_mon, "-", "01"))
    
    today_last_day_of_month <- ceiling_date(today_ymd, unit = "month") - days(1)
    
    df <-
      read.socrata(
        paste0(
          "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between ",
          "'",
          today_ymd,
          "' ",
          "and ",
          "'",
          today_last_day_of_month,
          "'"
        )
      )
    
    df <- df %>%
      mutate(longitude = as.numeric(longitude),
             latitude = as.numeric(latitude)) %>%
      filter(!is.na(longitude) & !is.na(latitude))
    
    
      tag.map.title <- tags$style(
        HTML(
        "
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
        "
      )
    )
    
    title <- tags$div(tag.map.title, 
                      HTML(paste(month_years[1], "Crime Heatmap")))
    
    leaflet(data = df) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addHeatmap(
        lng = ~ longitude,
        lat = ~ latitude,
        max = 5,
        radius = 40,
        blur = 25
      ) %>%
      addControl(title, position = "topright")
  })
  
  
  # update the leaflet heatmap to only use incidents for selected month
  observe({
    df <- selected_month_data()
    
    df <- df %>%
      mutate(longitude = as.numeric(longitude),
             latitude = as.numeric(latitude)) %>%
      filter(!is.na(longitude) & !is.na(latitude))
    
    tag.map.title <- tags$style(
      HTML(
        "
        .leaflet-control.map-title {
        transform: translate(-50%,20%);
        position: fixed !important;
        left: 50%;
        text-align: center;
        padding-left: 50px;
        padding-right: 50px;
        background: rgba(255,255,255,0.75);font-weight: bold;
        font-size: 1000px;
        }
        "
      )
    )
    
    title <- tags$div(tag.map.title, HTML(paste(input$select_month, "Crime Heatmap")))
    
    leafletProxy("map2", data = df) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      clearHeatmap() %>%
      addHeatmap(
        lng = ~ longitude,
        lat = ~ latitude,
        max = 5,
        radius = 40,
        blur = 25
      ) %>%
      clearControls() %>%
      addControl(title, position = "topright")
  })
  
  
  
  # CRIME ANALYSIS SECTION
  
  
  # create a function that makes an API call for the selected month in the Crime Analysis Section
  selected_month_data2 <- reactive({
    
    month_year_selected <- str_split(input$select_month2, " ")
    
    month_name_selected <- month_year_selected[[1]][1]
    
    year_selected <- month_year_selected[[1]][2]
    
    date_selected <-
      as.Date(str_replace_all(
        paste(year_selected, "-", month_name_selected, "-", "01"),
        " ",
        ""
      ), format = "%Y-%B-%d")
    
    last_day_month <- ceiling_date(date_selected, unit = "month") - days(1)
    
    read.socrata(
      paste0(
        "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between ",
        "'",
        date_selected,
        "' ",
        "and ",
        "'",
        last_day_month,
        "'"
      )
    )
  })
  
  # create a function that makes an API call for the selected month and selected neighborhood in the Crime Analysis Section
  selected_neighborhood_data <- reactive({
    
    month_year_selected <- str_split(input$select_month2, " ")
    
    month_name_selected <- month_year_selected[[1]][1]
    
    year_selected <- month_year_selected[[1]][2]
    
    date_selected <-
      as.Date(str_replace_all(
        paste(year_selected, "-", month_name_selected, "-", "01"),
        " ",
        ""
      ), format = "%Y-%B-%d")
    
    last_day_month <- ceiling_date(date_selected, unit = "month") - days(1)
    
    read.socrata(
      paste0(
        "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&",
        "analysis_neighborhood=",
        input$select_neighborhood,
        "&$where=incident_date between ",
        "'",
        date_selected,
        "' ",
        "and ",
        "'",
        last_day_month,
        "'"
      )
    )
  })
  
  # create an interactive plot for SF incident category counts
  output$incident_counts <- renderPlotly({
    
    df <- selected_month_data2()
    
    cats <- df %>%
      select(incident_category) %>%
      drop_na() %>%
      count(incident_category, sort = TRUE)
    
    p <- ggplot(cats, aes(x = reorder(incident_category, n),
                          y = n)) +
      geom_segment(aes(xend = incident_category,
                       yend = 0),
                   color = "gray") +
      geom_point(aes(text = paste(incident_category, "Count:", n)),
                 color = "red",
                 size = 2) +
      coord_flip() +
      theme_minimal() +
      xlab("") +
      ylab(paste("Number of Incidents in", input$select_month2)) +
      theme(axis.text.y = element_text(size = 6),
            plot.title = element_text(hjust = 0.5)) + 
      labs(title = "San Francisco")
    
    ggplotly(p, tooltip = "text")
    
  })
  
  # create an interactive plot for neighborhood incident category counts
  output$incident_counts_neighborhood <- renderPlotly({
    
    df <- selected_neighborhood_data()
    
    cats <- df %>%
      select(incident_category) %>%
      drop_na() %>%
      count(incident_category, sort = TRUE)
    
    p <- ggplot(cats, aes(x = reorder(incident_category, n),
                          y = n)) +
      geom_segment(aes(xend = incident_category,
                       yend = 0),
                   color = "gray") +
      geom_point(aes(text = paste(incident_category, "Count:", n)),
                 color = "red",
                 size = 2) +
      coord_flip() +
      theme_minimal() +
      xlab("") +
      ylab(paste("Number of Incidents in", input$select_month2)) +
      theme(axis.text.y = element_text(size = 6),
            plot.title = element_text(hjust = 0.5)) + 
      labs(title = input$select_neighborhood)
    
    ggplotly(p, tooltip = "text")
  })
  
  # create a barplot of the number of incidents in each hour in SF
  # allows user to see the most common incident category in each hour by hovering mouse
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
    
    full_df <-
      full_join(df_count_hour, df_most_common_incident, by = "incident_hour")
    
    full_df <- full_df %>%
      mutate(
        incident_hour_format = strftime(strptime(
          sprintf("%02d", incident_hour), format = "%H"
        ), format = "%I %p"),
        incident_hour_format = gsub("^0", "", incident_hour_format),
        incident_hour_format = gsub(" ", "", incident_hour_format)
      )
    
    p <- full_df %>%
      ggplot(aes(
        x = as.factor(incident_hour),
        y = number,
        fill = "lightblue"
      )) +
      geom_col(aes(
        text = paste("Most Common Incident Category:\n",
                     incident_category)
      )) +
      theme_minimal() +
      xlab("Hour of the Day") +
      ylab(paste("Number of Incidents in", input$select_month2)) +
      scale_x_discrete(labels = full_df$incident_hour_format) +
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5)) + 
      labs(title = "San Francisco")
    
    ggplotly(p, tooltip = "text")
  })
  
  # create a barplot of the number of incidents in each hour in neighborhood
  # allows user to see the most common incident category in each hour by hovering mouse
  output$hour_day_neighborhood <- renderPlotly({
    
    df <- selected_neighborhood_data()
    
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
    
    full_df <-
      full_join(df_count_hour, df_most_common_incident, by = "incident_hour")
    
    full_df <- full_df %>%
      mutate(
        incident_hour_format = strftime(strptime(
          sprintf("%02d", incident_hour), format = "%H"
        ), format = "%I %p"),
        incident_hour_format = gsub("^0", "", incident_hour_format),
        incident_hour_format = gsub(" ", "", incident_hour_format)
      )
    
    p <- full_df %>%
      ggplot(aes(
        x = as.factor(incident_hour),
        y = number,
        fill = "lightblue"
      )) +
      geom_col(aes(
        text = paste("Most Common Incident Category:\n",
                     incident_category)
      )) +
      theme_minimal() +
      xlab("Hour of the Day") +
      ylab(paste("Number of Incidents in", input$select_month2)) +
      scale_x_discrete(labels = full_df$incident_hour_format) +
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5)) + 
      labs(title = input$select_neighborhood)
    
    ggplotly(p, tooltip = "text")
    
  })
  
  # create an interactive plot of the incident counts in each neighborhood in SF
  output$neighborhood_counts <- renderPlotly({
    
    df <- selected_month_data2()
    
    cats <- df %>%
      select(analysis_neighborhood) %>%
      drop_na() %>%
      count(analysis_neighborhood, sort = TRUE)
    
    p <- ggplot(cats, aes(x = reorder(analysis_neighborhood, n),
                          y = n)) +
      geom_segment(aes(xend = analysis_neighborhood,
                       yend = 0),
                   color = "gray") +
      geom_point(aes(text = paste(analysis_neighborhood, "Count: ", n)),
                 color = "red",
                 size = 2) +
      coord_flip() +
      theme_minimal() +
      xlab("") +
      ylab(paste("Number of Incidents in", input$select_month2)) +
      labs(title = "Incident Count by Neighborhood") +
      theme(axis.text.y = element_text(size = 6),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  # CRIME DATA TABLE SECTION
  
  
  
  # create a function that makes an API call for the selected month and neighborhood
  selected_neighborhood_data_search <- reactive({
    
    month_year_selected <- str_split(input$select_month_search, " ")
    
    month_name_selected <- month_year_selected[[1]][1]
    
    year_selected <- month_year_selected[[1]][2]
    
    date_selected <-
      as.Date(str_replace_all(
        paste(year_selected, "-", month_name_selected, "-", "01"),
        " ",
        ""
      ), format = "%Y-%B-%d")
    
    last_day_month <- ceiling_date(date_selected, unit = "month") - days(1)
    
    read.socrata(
      paste0(
        "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&",
        "analysis_neighborhood=",
        input$select_neighborhood_search,
        "&$where=incident_date between ",
        "'",
        date_selected,
        "' ",
        "and ",
        "'",
        last_day_month,
        "'"
      )
    )
  })
  
  # create the data table output of the selected neighborhood data
  output$table_search <- renderDataTable({
    df <- selected_neighborhood_data_search()
    
    df %>%
      datatable()
  })

}


# Run the application
shinyApp(ui = ui, server = server)
