library(dplyr)
library(leaflet)
library(sp)
library(htmlwidgets)

data <- read.csv("vestex1.csv") 
data <- data[complete.cases(data)]

data$long <- as.numeric(data$long)
data$lat <- as.numeric (data$lat)

data.sp <- SpatialGridDataFrame(data [,c(2,3)], data [,-c(2,3)])

#Para addProv usar link https://leaflet-extras.github.io/leaflet-providers/preview/index.html entre comillas. "CartoDB.DarkMatterNoLabels"
guate <- leaflet() %>% addTiles()  %>%
  addMarkers(data = data, lng = ~long, lat = ~lat, popup = ~actividad) %>% addProviderTiles("OpenStreetMap.DE")

guate

saveWidget(guate, file="guate.html")

