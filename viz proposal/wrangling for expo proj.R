odata <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv", header = T)
head(odata)
str(odata)
library(tidyr)
library(tidyverse)

#ordering data by LocationDesc and year
orderdata <- odata[order(odata$LocationDesc,odata$YearStart,
                         odata$StratificationCategory1,odata$Question,odata$Class),]
#geolocation to strings
orderdata$GeoLocation <- as.character(orderdata$GeoLocation)
#removing comma and parenthesis
orderdata$GeoLocation<-gsub("[(),]", "", orderdata$GeoLocation)

# splitting geolocation into 2 columns latitude longitude
orderdata <- separate(data = orderdata, col = GeoLocation, into = c("latitude", "longitude"), sep = "\\ ")

#removing unwanted columns only taking columns i need
#age,education,income,gender,race columns are identifiable through stratification1 and ID 
#so removed them
orderdata<- orderdata[,c(1,3,4,6,8,11,12,15,16,17,18,24,25,31,32)]
#removing rows with Null values in data value column
orderdata<- orderdata[!is.na(orderdata$Data_Value),]

#reseting index for orderdata
row.names(orderdata)<-1:nrow(orderdata)

#renaming columns for order data
orderdata<- orderdata %>% 
    rename(
    States = LocationDesc,
    Percentage = Data_Value,
    Alt_percentage = Data_Value_Alt,
    Latitude = latitude,
    Longitude = longitude,
    Year = YearStart,
    State_Abbr = LocationAbbr
    )

#obese and overweight population
obese.overweight<- orderdata[orderdata$Class=="Obesity / Weight Status",]

# Question 3
# df for age affecting obesity question
age.obese<- obese.overweight[obese.overweight$StratificationCategory1=='Age (years)',]
#df for race affecting obesity question
race.obese <- obese.overweight[obese.overweight$StratificationCategory1=='Race/Ethnicity',]



#Question 4
# df for difference in levels of obesity by gender
gender.obese <- obese.overweight[obese.overweight$StratificationCategory1=='Gender',]

#activity level

physicalactivity<- orderdata[orderdata$Class=='Physical Activity'&orderdata$Question!='Percent of adults who engage in no leisure-time physical activity',]
gender.physicalactivity<- physicalactivity[physicalactivity$StratificationCategory1=='Gender',]



#Question 5 - factors infuencing unhealthy diet, corelation of income,education with diet





write.csv(orderdata,'orderdata.csv')










































#checking if year start and end are same
identical(odata[['YearStart']],odata[['YearEnd']])

#checking if Data_Value and Data_value_Alt are same
identical(odata[['Data_Value']],odata[['Data_value_Alt']])
library(compare)
compare(odata[['Data_Value']], odata[['Data_value_Alt']])






phys<-odata[odata$Class=="Physical Activity",]
head(phys)
str(phys)

#checking class and factor levels of questions
class(odata$Question)
levels(odata$Question)





# checking about states and territories - 50 states, 1 district, 3 terretories
class(odata$LocationDesc)
levels(odata$LocationDesc)
#Abbv - same
class(odata$LocationAbbr)
levels(odata$LocationAbbr)

#checking unique years - * 8 years worth of data
rapply(odata[1],function(x)length(unique(x)))
rapply(odata[2],function(x)length(unique(x)))

# rows with totals
total<-odata[odata$Total == "Total"& odata$LocationDesc=="Pennsylvania",]
# pennselvania totals ordered by year
To<- total[order(total$YearStart),]
To







