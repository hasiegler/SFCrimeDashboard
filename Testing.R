library(RSocrata)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(treemapify)

df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?analysis_neigborhood=Tenderloin&$where=incident_date between '2023-01-18' and '2023-02-01'"
)

df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?analysis_neigborhood=Tenderloin"
)

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
  tag.map.title, HTML(paste("Hey", "Map title"))
) 

leaflet(data = df) %>% 
  addTiles() %>% 
  addHeatmap(lng= ~longitude, lat = ~latitude, max = 5, radius = 40, blur = 25)


df <- df %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>% 
  filter(!is.na(longitude) & !is.na(latitude))

mon <- month(Sys.Date())
y <- year(Sys.Date())
datestring <- str_replace_all(paste(y,"-", sprintf("%02d", mon),"-","01"), " ","")
date<- as.Date(datestring)
start_date <- as.Date('2018-01-01')
end_date <- date
dates <- seq(start_date, end_date, by = "month")
months <- format(dates, "%m")
months_words <- format(dates, "%B")
years <- format(dates, "%Y")
month_years <- paste(months, years)
month_years





leaflet(data = df) %>% 
  addTiles() %>% 
  addMarkers(~longitude, ~latitude)



df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?$where=incident_date between '2022-06-01' and '2023-02-15'")




all <- df %>% 
  count(incident_category, sort = TRUE) %>% 
  pull(incident_category)

print()

for (i in all){
  print(i)
}

test <- yesterday_df %>% 
  filter(incident_category == "Larceny Theft")

test %>% 
  ggplot(aes(x=analysis_neighborhood)) + 
  geom_bar()






df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&analysis_neighborhood=Tenderloin&$where=incident_date between '2023-01-01' and '2023-02-01'"
)


df %>% 
  count(resolution)

cats <- df %>% 
  filter(resolution == "Cite or Arrest Adult") %>% 
  count(incident_category, sort = TRUE)

ggplot(cats, aes(area = n, fill = incident_category, label = incident_category)) +
  geom_treemap() +
  geom_treemap_text()

p <- ggplot(cats, aes(x = reorder(incident_category, n),
                 y = n,
                 text = paste(incident_category, "Count: ", n))) +
  geom_segment(aes(xend = incident_category,
                   yend = 0),
               color = "gray",
               size = 0.7) + 
  geom_point(color = "red",
             size = 2) + 
  coord_flip() + 
  theme_minimal() + 
  xlab("") + 
  ylab("Number of Incidents")

ggplotly(p, tooltip = "text")




df <- df %>% 
  mutate(incident_date = as.Date(incident_date))

df %>% 
  sum

month(df$incident_date)

df %>% 
  mutate(month = month(df$incident_date)) %>% 
  filter(incident_category == "Robbery") %>% 
  ggplot(aes(x = month)) + geom_bar()


  ggplot(aes(x = incident_date)) + 
  geom_bar()


  
  
  
  
  
df <- read.socrata(
    "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between '2023-01-01' and '2023-02-01'"
  )
  

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



df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?resolution=Cite or Arrest Adult&$where=incident_date between '2022-01-01' and '2023-02-01'"
)

df %>% 
  count(analysis_neighborhood, sort = TRUE) %>% 
  pull(analysis_neighborhood)








