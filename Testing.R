library(RSocrata)
library(tidyverse)
library(leaflet)


df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?incident_date=2022-02-11"
)

df <- df %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

leaflet(data = df) %>% 
  addTiles() %>% 
  addMarkers(~longitude, ~latitude)



