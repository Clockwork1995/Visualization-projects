library(ggplot2)
theme_set(theme_classic())
library(dplyr)
library(usmap)
library("tibble")


#retrying tableau vizzes with ggplot2 and usmap

orderlatlong<- orderdata[,c(12,13)]

col_order <- c("longitude", "latitude")
orderlatlong <- orderlatlong[, col_order]
orderlatlong

orderlatlong<- orderlatlong %>% 
  rename(
    longitude = Longitude,
    latitude = Latitude
  )

#converting to numeric
orderlatlong[] <- lapply(orderlatlong, function(x) as.numeric(as.character(x)))

orderlatlong<- orderlatlong[!is.na(orderlatlong$latitude&orderlatlong$longitude),]

orderalbers<- usmap_transform(orderlatlong)

orderalbers<- orderalbers %>% 
  rename(
    Longitude = longitude,
    Latitude = latitude
  )



plot_usmap() +
  geom_point(data = orderalbers, aes(x = longitude.1, y = latitude.1),
             color = "red", alpha = 0.25) +
  labs(title = "US Earthquakes",
       subtitle = "Source: USGS, Jan 1 to Jun 30 2019",
       size = "Magnitude") +
  theme(legend.position = "right")

