
library(sp)
library(albersusa)

s_proj <- usa_composite()
s_proj1 <- usa_composite()
# to check if the state names in the shape file matches with my data
is.element(obese_overweight$States,s_proj$name)

#checking if opposite is true
is.element(s_proj$name,obese_overweight$States)

s_proj <- subset(s_proj, is.element(s_proj$name, obese_overweight$States))
#align the data across both the datasets
obese_overweight_alt <- obese_overweight[order(match(obese_overweight$States,s_proj$name)),]

#same for physical activity

s_proj1 <- subset(s_proj1, is.element(s_proj1$name, physical$States))
#align the data across both the datasets
physical_alt <- physical[order(match(physical$States,s_proj1$name)),]



bounds <- c(-125, 24 ,-75, 45)


kk <- leaflet(options=
               leafletOptions(
                 worldCopyJump = FALSE,
                 crs=leafletCRS(
                   crsClass="L.Proj.CRS",
                   code='EPSG:2163',
                   proj4def='+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs',
                   resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128)
                 ))) %>%
  fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
  setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
  
  setView(lng = -94.676392, lat = 39.106667, zoom = 2)%>% #centre on kansas
  addPolygons(data = s_proj,
              fillOpacity = 0.75,
              fillColor = palette(obese_overweight_alt$mean),
              color="white",
              weight = 1,
              popup = popup1,
              group="<span style='color: #7f0000; font-size: 10pt'><strong>Obesity Map</strong></span>")%>%
  addPolygons(data = s_proj1,
              fillOpacity = 0.75,
              fillColor = palette1(physical_alt$mean),
              color="white",
              weight = 1,
              popup = popup2,
              group="Physical Activity Map")%>%
  
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 10pt'><strong>Obesity Map</strong></span>",
                   "<span style='color: #084081; font-size: 10pt'><strong>Physical Activity Map</strong></span>"),
    options = layersControlOptions(collapsed = FALSE))


kk



