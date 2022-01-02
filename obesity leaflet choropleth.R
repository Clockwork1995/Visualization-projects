library(rgdal)
library(leaflet)
library(dplyr)
library(sp)
library(albersusa)
library(raster)


us_data <- read.csv("orderdata.csv")

#removing National
us_data = filter(us_data, States!= "National")


#only obesity data from all the data
obese_overweight<- us_data[us_data$Class=="Obesity / Weight Status",]

#average obesity for each state for map
obese_overweight <- obese_overweight %>%
  group_by(States) %>% summarise(mean = mean(Percentage))
  








usa_states = readOGR("us_states/cb_2019_us_state_5m/cb_2019_us_state_5m.shp")
#plot(usa_states)

# to check if the state names in the shape file matches with my data
is.element(obese_overweight$States,usa_states$NAME)

#checking if opposite is true
is.element(usa_states$NAME,obese_overweight$States)

levels(obese_overweight$States[49])
levels(usa_states$NAME[49])

#changing name
levels(obese_overweight$States)[50] <- "United States Virgin Islands"

#making sure both the data sets have matching state names
usa_states <- subset(usa_states, is.element(usa_states$NAME, obese_overweight$States))



#align the data across both the datasets
obese_overweight <- obese_overweight[order(match(obese_overweight$States,usa_states$NAME)),]

#creating bins for different shades of colors:
#color scheme for choropleth

palette <- colorBin(c('#fff7ec',  
                      '#fee8c8',
                      '#fdd49e',
                      '#fdbb84',
                      '#fc8d59',
                      '#ef6548',
                      '#d7301f',
                      '#b30000',
                      '#7f0000'), 
                    bins = c(27,28,29,30,31,32,33,34,35,36))



z <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner)%>%
  setView(lng = -94.676392, lat = 39.106667, zoom = 2)%>% #centre on kansas
  addPolygons(data = usa_states,
              fillOpacity = 0.65,
              fillColor = palette(obese_overweight$mean),
              color="white",
              weight = 1)
z

#popups

#for obesity
popup1 <- paste0("<span style='color: #7f0000'><strong>Obesity levels</strong></span>",
                 "<br><span style='color: #ef6548;'><strong>State:</strong></span>", 
                 obese_overweight$States, 
                 "<br><span style='color: #ef6548;'><strong>Percentage</strong></span>", 
                 round(obese_overweight$mean,2))



  
  

