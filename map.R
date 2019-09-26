
library(leaflet)
library(htmlwidgets)

setwd("C:/Users/JSpector/Documents/Delta Imports Exports Shiny App")

stations <- read.csv("Station_Coordinates.csv")

map <- leaflet(options=leafletOptions(preferCanvas=TRUE)) %>% addProviderTiles(providers$Esri.WorldTopo) %>% addMarkers(data=stations, label=~Name)

saveWidget(map, "map.html")
