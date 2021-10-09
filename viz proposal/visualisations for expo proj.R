library(ggplot2)
theme_set(theme_classic())
library(dplyr)
library(usmap)



raceobesepercentage <- aggregate( Percentage ~ Stratification1, race.obese, mean )
raceobesepercentage

raceobesepercentage<- raceobesepercentage %>% 
  rename(
    Ethnicity = Stratification1
    )

write.csv(raceobesepercentage,'raceobesepercentage.csv')

# Plot
race <- ggplot(raceobesepercentage, aes(Ethnicity, Percentage))
race + geom_bar(stat="identity", width = 0.7, aes(color = Ethnicity)) + 
  labs(title="Effect of Race on obesity",
       caption="Source: Percentage of obese/overweight from cleaned data set") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))



ageobesepercentage <- aggregate( Percentage ~ Stratification1, age.obese, mean )
ageobesepercentage


ageobesepercentage<- ageobesepercentage %>% 
  rename(
    Age_range = Stratification1
  )


write.csv(ageobesepercentage,'ageobesepercentage.csv')

# Plot
age <- ggplot(ageobesepercentage, aes(Age_range, Percentage))
age + geom_bar(stat="identity", width = 0.7, aes(color=Age_range)) + 
  labs(title="Effect of Age on obesity",
       caption="Source: Percentage of obese/overweight from cleaned data set") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))


# obese by gender for all years
genderobesepercentage <- aggregate( Percentage ~ Stratification1+Year, gender.obese, mean )
genderobesepercentage


genderobesepercentage<- genderobesepercentage %>% 
  rename(
    Gender = Stratification1
  )
#year to factor
genderobesepercentage[,'Year']<-factor(genderobesepercentage[,'Year'])



write.csv(genderobesepercentage,'genderobesepercentage.csv')


# Plot


ggplot(genderobesepercentage, aes(fill = Gender, y = Percentage, x = Year)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values=c("#CC79A7", "#0072B2")) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10)) +
  labs (y= "Obese percentage\n", 
        x = "\nYears", 
        fill = "Gender", 
        title = "Levels of obesity in USA by gender\n")

# activity level of genders

genderphyspercentage<- aggregate( Percentage ~ Stratification1+Year, gender.physicalactivity, mean )

genderphyspercentage<- genderphyspercentage %>% 
  rename(
    Gender = Stratification1
  )

#year to factor
genderphyspercentage[,'Year']<-factor(genderphyspercentage[,'Year'])
#plot


ggplot(genderphyspercentage, aes(fill = Gender, y = Percentage, x = Year)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values=c("#CC79A7", "#0072B2")) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10)) +
  labs (y= "Physical activity percentage\n", 
        x = "\nYears", 
        fill = "Gender", 
        title = "Levels of physical activity in USA by gender\n")

#Males have higher levels of physical activity than females thus their obesity levels are increasing slower

#also try and include diet into the equation if you have time 




#question5
#factors affecting unhealthy diet


#people who dont eat fruits veggies
unhealthy<- orderdata[orderdata$Class=='Fruits and Vegetables',]

unhealthyper <- aggregate(Percentage ~ Stratification1, unhealthy, mean)

write.csv(unhealthyper,'unhealthyper.csv')  



#Relation of obesity and unhealthy diet
  
obeseagg<- aggregate(Percentage~States, obese.overweight,mean)
unhealthyagg <- aggregate(Percentage ~ States, unhealthy, mean)

unhealthyagg<- unhealthyagg %>% 
  rename(
    unhealthy_Diet = unhealthy_Percentage
  )

obese.unhealthy <- merge(obeseagg, unhealthyagg, by="States", all = T)

obese.unhealthy<- obese.unhealthy[obese.unhealthy$States!='Virgin Islands',]

#plot
ob_unhealthy <- ggplot(obese.unhealthy, aes(x=Percentage, y=unhealthy_Diet)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs(x='Obesity levels', y='unhealthy food habits',
       title = 'Relation of obesity and unhealthy diet')
  

ob_unhealthy

#positive correlation between obesity and unhealthy diet

#retrying tableau vizzes with ggplot2 and usmap








