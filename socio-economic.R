library(RColorBrewer)
n <- 55
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))




#socio economic factors:

#age
age<- subset(us_data, StratificationCategory1 == "Age (years)")

#age obesity
age_obese<- subset(age, Class == "Obesity / Weight Status")

age_obese<- age_obese %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#age physical
age_physical<- subset(age, Class == "Physical Activity"&
                        Question!='Percent of adults who engage in no leisure-time physical activity')

age_physical<- age_physical %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#Education

education <- subset(us_data, StratificationCategory1 =="Education")

#education obesity

education_obese<- subset(education, Class == "Obesity / Weight Status")

education_obese<- education_obese %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#education physical activity

education_physical<- subset(education, Class == "Physical Activity"&
                        Question!='Percent of adults who engage in no leisure-time physical activity')

education_physical<- education_physical %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))


#Gender

gender <- subset(us_data, StratificationCategory1 =="Gender")

#gender obesity

gender_obese<- subset(gender, Class == "Obesity / Weight Status")

gender_obese<- gender_obese %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#gender physical activity

gender_physical<- subset(gender, Class == "Physical Activity"&
                              Question!='Percent of adults who engage in no leisure-time physical activity')

gender_physical<- gender_physical %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))


#Income

income <- subset(us_data, StratificationCategory1 =="Income")

#income obesity

income_obese<- subset(income, Class == "Obesity / Weight Status")

income_obese<- income_obese %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#income physical activity

income_physical<- subset(income, Class == "Physical Activity"&
                           Question!='Percent of adults who engage in no leisure-time physical activity')

income_physical<- income_physical %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#Race/Ethnicity

race<- subset(us_data, StratificationCategory1 =="Race/Ethnicity")

#Race obesity

race_obese<- subset(race, Class == "Obesity / Weight Status")

race_obese<- race_obese %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#race physical activity

race_physical<- subset(race, Class == "Physical Activity"&
                           Question!='Percent of adults who engage in no leisure-time physical activity')

race_physical<- race_physical %>%
  group_by(Stratification1) %>% summarise(mean = mean(Percentage))















