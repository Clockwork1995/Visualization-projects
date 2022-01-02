# R Assignment


library(ggplot2)

#Reading the coral data from the csv data file
coraldata <- read.csv("assignment-02-data-formated.csv", header = T)
head(coraldata)

#structure f the data
str(coraldata)

# deleting % from th end of the values in the value column and 
# converting factor value column into numeric
coraldata[,6] <- as.numeric(sub("%","",coraldata[,6]))
coraldata

#facet grids ordered by latitude(ascending order of latitude increasing) by turning the sites into factor with levels
coraldata$location <- factor(coraldata$location,
                             levels = c("site04","site02","site06","site08",
                                        "site07","site05","site01","site03"))
                             


                             
# creating the ggplot 
bc <- ggplot(coraldata,aes(year,value))+
  geom_point(aes(color = coralType))+
  #facets showing coral types for each site ordered by latitude increasing
  facet_grid(coralType~location+latitude, scales="free")+
  labs(x="Year", y="Coral Bleaching Percentage", title = "Variations in bleaching for Corals 
       over the years by type and location")+
  #showing line smoothing
  stat_smooth(method = lm, color = "black", size = 0.4) 

bc


