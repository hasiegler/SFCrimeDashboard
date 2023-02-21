library(RSocrata)
library(tidyverse)
library(leaflet)


df <- read.socrata(
  "https://data.sfgov.org/resource/wg3w-h783.json?incident_date=2023-02-18&incident_category=Larceny Theft"
)

df <- df %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

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







