physical<- us_data[us_data$Class=='Physical Activity'&us_data$Question!='Percent of adults who engage in no leisure-time physical activity',]

#average activity for each state for map
physical <- physical %>%
  group_by(States) %>% summarise(mean = mean(Percentage))

# to check if the state names in the shape file matches with my data
is.element(physical$States,usa_states$NAME)

#checking if opposite is true
is.element(usa_states$NAME,physical$States)

#making sure both the data sets have matching state names (new variable for physical)
usa_states1 <- subset(usa_states, is.element(usa_states$NAME, physical$States))

#align the data across both the datasets
physical <- physical[order(match(physical$States,usa_states1$NAME)),]


#creating bins for different shades of colors:
#color scheme for choropleth

palette1 <- colorBin(c('#f7fcf0',  
                      '#e0f3db',
                      '#ccebc5',
                      '#a8ddb5',
                      '#7bccc4',
                      '#4eb3d3',
                      '#2b8cbe',
                      '#0868ac',
                      '#084081'), 
                    bins = c(15,18,21,24,27,30,33,36,39.5))

#leaflet

zz <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner)%>%
  setView(lng = -94.676392, lat = 39.106667, zoom = 2)%>% #centre on kansas
  addPolygons(data = usa_states1,fillOpacity = 0.65,fillColor = palette1(physical$mean),color="white", weight = 1)
zz

#popup

#for physical activity
popup2 <- paste0("<span style='color: #084081'><strong>Physical Activity levels</strong></span>",
                 "<br><span style='color: #4eb3d3;'><strong>State:</strong></span>", 
                 physical$States, 
                 "<br><span style='color: #4eb3d3;'><strong>Percentage</strong></span>", 
                 round(physical$mean,2))





