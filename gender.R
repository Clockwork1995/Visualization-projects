library(viridis)
library(hrbrthemes)



#for gender obese and gender physical for all years:

gender_obese_bar<- subset(gender, Class == "Obesity / Weight Status")

gender_obese_bar<- gender_obese_bar %>%
  group_by(Stratification1,Year) %>% summarise(mean = mean(Percentage))

gender_obese_bar<- gender_obese_bar %>% 
  rename(
    Gender = Stratification1
  )

gender_obese_bar$Year <- as.factor(gender_obese_bar$Year)



gender_physical_bar<- subset(gender, Class == "Physical Activity"&
                           Question!='Percent of adults who engage in no leisure-time physical activity')

gender_physical_bar<- gender_physical_bar %>%
  group_by(Stratification1,Year) %>% summarise(mean = mean(Percentage))


gender_physical_bar<- gender_physical_bar %>% 
  rename(
    Gender = Stratification1
  )

gender_physical_bar$Year <- as.factor(gender_physical_bar$Year)


#for obesity

ggplot(gender_obese_bar, aes(fill = Gender, y = mean, x = Year)) + 
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



#for physical activity

ggplot(gender_physical_bar, aes(fill = Gender, y = mean, x = Year)) + 
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



#correlation
unhealthy<- us_data[us_data$Class=='Fruits and Vegetables',]

#Relation of obesity and unhealthy diet

obeseagg<- aggregate(Percentage~States, us_data[us_data$Class=="Obesity / Weight Status",],mean)
unhealthyagg <- aggregate(Percentage ~ States, unhealthy, mean)



obese.unhealthy <- merge(obeseagg, unhealthyagg, by="States", all = T)

obese.unhealthy<- obese.unhealthy[obese.unhealthy$States!='Virgin Islands',]
obese.unhealthy<- obese.unhealthy[obese.unhealthy$States!='Puerto Rico',]

obese.unhealthy<- obese.unhealthy %>% 
                  rename(
                    obesity = Percentage.x,
                    unhealthy_food_habits = Percentage.y
                  )

#bubblechart plotly

fig <- plot_ly(obese.unhealthy, x = ~unhealthy_food_habits, y = ~obesity, text = ~States, type = 'scatter', mode = 'markers', size = ~(abs(obesity-unhealthy_food_habits)), color = ~States, colors = 'Paired',
               #Choosing the range of the bubbles' sizes:
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter'))
fig <- fig %>% layout(title = 'relation of obesity and unhealthy food habits ',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE),
                      showlegend = FALSE)

fig


#gender area chart

Female<-gender_obese_bar[c(1:8),]
Male<- gender_obese_bar[c(9:16),]

male_female_obese <- merge(Female, Male, by="Year", all = T)


# Plot
plot_ly(male_female_obese, x = ~Year, y = ~mean.x, name = 'Female',
        type = 'scatter', mode = 'lines+markers') %>% 
  add_trace(y = ~mean.y, name = 'Male', mode = 'lines+markers')%>%
  layout(title = "Obesity levels over the years by Gender",
         yaxis = list (title = "Percentage(%)"))
  



