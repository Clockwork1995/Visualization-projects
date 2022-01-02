





k <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner)%>%
  setView(lng = -94.676392, lat = 39.106667, zoom = 3)%>% #centre on kansas
  addPolygons(data = usa_states,
              fillOpacity = 0.65,
              fillColor = palette(obese_overweight$mean),
              highlightOptions = highlightOptions(color = "red", weight = 5,
                                                  bringToFront = TRUE),
              color="white",
              weight = 1,
              popup = popup1,
              group="<span style='color: #7f0000; font-size: 10pt'><strong>Obesity Map</strong></span>")%>%
  addPolygons(data = usa_states1,fillOpacity = 0.65,
              fillColor = palette1(physical$mean),
              highlightOptions = highlightOptions(color = "blue", weight = 5,
                                                  bringToFront = TRUE),
              color="white",
              weight = 1,
              popup = popup2,
              group="Physical Activity Map")%>%
  
addLayersControl(
  baseGroups = c("<span style='color: #7f0000; font-size: 10pt'><strong>Obesity Map</strong></span>",
                 "<span style='color: #084081; font-size: 10pt'><strong>Physical Activity Map</strong></span>"),
  options = layersControlOptions(collapsed = FALSE))


k





