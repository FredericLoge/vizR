library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(htmltools)

## geom_sf =====

us_geo <- tigris::states(class = "sf", cb = TRUE) %>% 
  shift_geometry() %>%
  mutate(wages = rnorm(n = n()))

g = us_geo %>% 
  ggplot(aes(fill = wages, color = wages)) +
  geom_sf() +
  coord_sf(crs = 5070, datum = NA) +
  scale_fill_viridis_c(direction = -1, label = scales::dollar) + 
  scale_color_viridis_c(direction = -1, label = scales::dollar) +
  labs(title = "Annual Mean Wages by State",
       subtitle = "Mental Health and Substance Abuse Social Workers(SOC Code211023)", 
       caption = "Data Source: BLS.gov - Occupational Employment Statistics ; 2016")
ggplotly(g)


## leaflet ======


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, 
             popup="The birthplace of R")
m  

df <- read.csv(textConnection(
  "Name,Lat,Long
Samurai Noodle,47.597131,-122.327298
Kukai Ramen,47.6154,-122.327157
Tsukushinbo,47.59987,-122.326726"
))

leaflet(df) %>% addTiles() %>%
  addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name),
             clusterOptions = markerClusterOptions())

### mapview =========

library("mapview")
# alternative ? remotes::install_github("r-spatial/mapview")

mapview(breweries)
