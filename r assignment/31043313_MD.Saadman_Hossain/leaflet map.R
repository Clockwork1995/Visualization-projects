# leaflet for assignment
#loading required libraries


library(leaflet)
library(maps)

coraldata <- read.csv("assignment-02-data-formated.csv", header = T)

newdat <- coraldata[,c(1,3,4)]
newdat

mm <- leaflet(data = newdat) %>% 
  #using a provider tile terrain from stamen maps
  addProviderTiles("Stamen.Terrain") %>%
  #clicking on popups shows site names
  addMarkers(~longitude, ~latitude, label = ~as.character(location),
             labelOptions = labelOptions(noHide = TRUE, direction = "left",textsize='8px',
                                         style=list(
                                           'color'= "black",
                                           'font-family'= 'serif',
                                           'font-style'= 'italic',
                                           'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                           'font-size' = '9px',
                                           'border-color' = 'rgba(0,0,0,0.5)'
                                         ))
             )
  


  

mm


