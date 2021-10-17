library(tidyverse)
library(leaflet)
library(leaflet.extras)

####################
### AwesomeIcons ###
####################
statidum <- awesomeIcons(
  icon = 'star',
  iconColor = 'black',
  library = 'ion',
  markerColor = 'red'
)

############################
### Location information ###
############################
white_sox = data.frame("Chicago White Sox",41.830066,-87.634833)
colnames(white_sox) = c("Location", "Lat", "Lon")
City = data.frame("Chicago",41.830066,-87.634833)
colnames(City) = c("City", "lattitude", "longtitude")


leaflet() %>% 
  addTiles() %>% 
  setView(lng = City$longtitude, lat = City$lattitude, zoom = 13) %>% 
  addAwesomeMarkers(white_sox$Lon,white_sox$Lat,icon = statidum,group = 'Chicago White Sox',label = "Chicago White Sox",labelOptions = labelOptions(noHide = FALSE)) %>%
  addCircles(group="Surrounding Area",lng = white_sox$Lon,lat=white_sox$Lat,col = "blue", opacity=0.3, fillOpacity =0.3, radius=2000)

