# required libraries are loaded


library(shiny)
require(leaflet)
require(ggplot2)

coraldata <- read.csv("assignment-02-data-formated.csv", header = T)

# deleting % from th end of the values in the value column and 
# converting factor value column into numeric
coraldata[,6] <- as.numeric(sub("%","",coraldata[,6]))

#facet grids ordered by latitude(ascending order of latitude increasing) 
#by turning the sites into factor with certain levels
coraldata$location <- factor(coraldata$location,
                             levels = c("site04","site02","site06","site08",
                                        "site07","site05","site01","site03"))



# new data frame with location, longitude and latitude columns for leaflet map
newdat <- coraldata[,c(1,3,4)]
newdat




# Define UI for variations in bleaching for types of corals application
shinyUI <- (fluidPage( 
    
    # Application title
    headerPanel("Trend in coral bleaching as latitude increases 
                and leaflet map for the great barrier reef"),
    
    # Sidebar with 2 selection inputs 1)to select the corals to plot, 2)choice of smoothers
    
    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Type of Corals:", 
                        c("Blue Corals" = "blue corals", 
                          "Hard Corals" = "hard corals",
                          "Sea Fans" = "sea fans",
                          "Sea Pens" = "sea pens",
                          "Soft Corals" = "soft corals")
            ),
            selectInput("smoother", "Choice of smoothers",
                        c("lm"="lm","loess"="loess","glm" = "glm")
            )
            
            
        ),
        
        # Show the caption and plot of the requested coral types also plot the leaflet map
        mainPanel(
            h3(textOutput("caption")),
            plotOutput("plot1"),
            leafletOutput("mymap")
        )
    )
))

# server.R

# Define server logic required to plot selection of corals
shinyServer <- (function(input, output) {
    
    # Return the formula text for printing as a caption
    output$caption <- reactiveText(function() {
        paste("variations in", input$variable)
    })
    
    # Generate a plot of variations in bleaching for requested coral types 
    
    # ggplot version
    
    output$plot1 <- reactivePlot(function() {
        # check for the input variable
        # for each corals types takings subsets of coraldata as a dataframe for plotting
        if (input$variable == "blue corals") {
        
            cdata <- subset(coraldata, coralType == "blue corals")
        }
        
        else if (input$variable == "hard corals") {
            
            cdata <- subset(coraldata, coralType == "hard corals")
        }
        
        else if (input$variable == "sea fans") {
            
            cdata <- subset(coraldata, coralType == "sea fans")
        }
        
        else if (input$variable == "sea pens") {
            
            cdata <- subset(coraldata, coralType == "sea pens")
        }
        
        else {
            
            cdata <- subset(coraldata, coralType == "soft corals")
            
        }
        
        
        bc <- ggplot(cdata,aes(year,value))+
            geom_point(aes(color = location))+
            # facets are ordered by latitude of sites and shows the trend as latitude increases
            facet_grid(coralType~location+latitude, scales="free")+
            #creating labels
            labs(x="Year", y="Coral Bleaching Percentage", title = "Variations in bleaching for Corals 
            over the years by type and location")+
            # the method selected is entered from the variable smoother in the selectInput function
            geom_smooth(method = input$smoother, color = "black", size = 0.4) 
        print(bc)
        
        # interactive leaflet map using newdat dataframe
        output$mymap <- renderLeaflet({ # create leaflet map
            mm <- leaflet(data = newdat) %>% 
                # using the terrain tile from stamen maps
                addProviderTiles("Stamen.Terrain") %>%
                #adding markers to signify the site locations,
                #it shows site names when hovered with mice.
                addMarkers(~longitude, ~latitude, label = ~as.character(location),
                           labelOptions = labelOptions(noHide = TRUE, direction = "left",textsize='8px',
                                                       style=list(
                                                           'color'= "black",
                                                           'font-family'= 'serif',
                                                           'font-style'= 'italic',
                                                           'box-shadow' = '2px 2px rgba(0,0,0,0.25)',
                                                           'font-size' = '9px',
                                                           'border-color' = 'rgba(0,0,0,0.5)'
                                                       )))
                
            print(mm)
        })
        
        
    })
    
})



# Run the interactive shiny application 
shinyApp(ui = shinyUI, server = shinyServer)




